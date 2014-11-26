package io.github.ptitjes.hmm.trainers

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Features._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.decoders.FullDecoder
import io.github.ptitjes.hmm.trainers.features.BaseFeatures

import scala.collection._

object DiscriminantTrainer extends Trainer.Factory {

	def name: String = "Disc"

	override def parameters: Set[Parameter[_]] = Set(ORDER, FEATURES, ITERATION_COUNT, AVERAGING, DECODER)

	object TrainerParameter

	object FEATURES extends ScalaObjectParameter[FeatureSetTemplate]("", c => BaseFeatures) {

		def formatValue(value: FeatureSetTemplate): String = value.name
	}

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	val NO_AVERAGING = 0
	val PARTIAL_AVERAGING = 1
	val COMPLETE_AVERAGING = 2

	object AVERAGING extends IntParameter("Averaging", 0) {
		override def formatValue(value: Int): String = value match {
			case NO_AVERAGING => "No"
			case PARTIAL_AVERAGING => "Partial"
			case COMPLETE_AVERAGING => "Complete"
		}
	}

	object DECODER extends ScalaObjectParameter[Decoder.Factory]("", c => FullDecoder) {

		def formatValue(value: Decoder.Factory): String = value.name
	}

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer with IterativeTrainer {

		val order = configuration(ORDER)
		val features = configuration(FEATURES)
		val iterationCount = configuration(ITERATION_COUNT)
		val useAveraging = configuration(AVERAGING)
		val decoderFactory = configuration(DECODER)

		def train(corpus: Corpus[Sequence with Annotation], callback: IterationCallback): Unit = {
			val breadth = stateCount(corpus)

			var allWeightPairs = mutable.ArrayBuffer[(Weights, Weights)]()
			val weightFactory: BitSet => (Weights, Weights) = {
				tags =>
					val weights = new Weights(tags, Array.ofDim[Double](breadth))
					val averagedWeights = new Weights(tags, Array.ofDim[Double](breadth))
					val weightPair = (weights, averagedWeights)
					allWeightPairs += weightPair
					weightPair
			}

			val (wordOnlyFeatures, otherFeatures, dictionary) =
				features.buildFeatures(breadth, order, corpus, weightFactory)

			val hmm = HMMDiscriminant(breadth, order,
				wordOnlyFeatures.map { case (weights, averagedWeights) => weights},
				otherFeatures.map { case (weights, averagedWeights) => weights},
				dictionary
			)

			val decoder = decoderFactory.instantiate(hmm, configuration)

			val sequences = corpus.sequences
			for (i <- 1 to iterationCount) {

				val (_, elapsedTime) = timed {
					val progress = new ProgressBar(f"Iteration $i%2d/$iterationCount%2d", sequences.length)
					progress.set(0)

					sequences.foreach { refSeq: Sequence with Annotation =>

						val hypSeq = decoder.decode(refSeq)

						if (refSeq.observablesAndStates.length != hypSeq.observablesAndStates.length) {
							throw new IllegalStateException("Observable length mismatch!")
						}

						val refIterator = refSeq.annotatedIterator(breadth, order)
						val hypIterator = hypSeq.annotatedIterator(breadth, order)
						while (refIterator.hasNext) {
							val (oRef, sRef) = refIterator.next()
							val (oHyp, sHyp) = hypIterator.next()

							if (oRef != oHyp) {
								throw new IllegalStateException("Observable mismatch!")
							}

							if (sRef != sHyp || refIterator.sourceState != hypIterator.sourceState) {
								val h_ref = refIterator.history
								val h_hyp = hypIterator.history

								val refTagFilter = BitSet(sRef)
								val hypTagFilter = BitSet(sHyp)

								wordOnlyFeatures.foreachMatching(h_ref, refTagFilter) {
									case (weights, _) => weights(sRef) += 1.0
								}
								wordOnlyFeatures.foreachMatching(h_hyp, hypTagFilter) {
									case (weights, _) => weights(sHyp) -= 1.0
								}

								otherFeatures.foreachMatching(h_ref, refTagFilter) {
									case (weights, _) => weights(sRef) += 1.0
								}
								otherFeatures.foreachMatching(h_hyp, hypTagFilter) {
									case (weights, _) => weights(sHyp) -= 1.0
								}
							}
						}

						if (useAveraging == COMPLETE_AVERAGING) {
							allWeightPairs.par.foreach {
								case (weights, averagedWeights) =>
									weights.foreach { case (tag, weight) => averagedWeights(tag) += weight}
							}
						}

						progress.increment()
					}

					if (useAveraging == PARTIAL_AVERAGING) {
						allWeightPairs.par.foreach {
							case (weights, averagedWeights) =>
								weights.foreach { case (tag, weight) => averagedWeights(tag) += weight}
						}
					}
				}

				callback.iterationDone(configuration.set(ITERATION_COUNT, i),
					if (useAveraging == NO_AVERAGING) hmm
					else {
						val averagingDivider = i * (if (useAveraging == COMPLETE_AVERAGING) sequences.size else 1)

						HMMDiscriminant(breadth, order,
							wordOnlyFeatures.map { case (weights, averagedWeights) =>
								averagedWeights.map(w => w / averagingDivider)
							},
							otherFeatures.map { case (weights, averagedWeights) =>
								averagedWeights.map(w => w / averagingDivider)
							},
							dictionary
						)
					},
					elapsedTime
				)
			}
		}
	}

}

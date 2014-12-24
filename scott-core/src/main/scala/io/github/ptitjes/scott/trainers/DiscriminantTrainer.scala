package io.github.ptitjes.scott.trainers

import io.github.ptitjes.scott.corpora.Annotation.{Form, CoarsePosTag}
import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.Features._
import io.github.ptitjes.scott.Trainer._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.decoders.FullDecoder
import io.github.ptitjes.scott.trainers.features.BaseFeatures
import io.github.ptitjes.scott.utils.DepthCounter
import io.github.ptitjes.scott.utils.States.SourceTracker

import scala.collection._

object DiscriminantTrainer extends Trainer.Factory {

	def name: String = "Perceptron"

	override def parameters: Set[Parameter[_]] = Set(ORDER, FEATURES, ITERATION_COUNT, AVERAGING, DECODER)

	object TrainerParameter

	object FEATURES extends ScalaObjectParameter[FeatureSetTemplate]("Features", c => BaseFeatures) {
		override def format(value: FeatureSetTemplate): String = value.name
	}

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	val NO_AVERAGING = 0
	val PARTIAL_AVERAGING = 1
	val COMPLETE_AVERAGING = 2

	object AVERAGING extends IntParameter("Averaging", 0) {
		override def format(value: Int): String = value match {
			case NO_AVERAGING => "No"
			case PARTIAL_AVERAGING => "Partial"
			case COMPLETE_AVERAGING => "Complete"
		}
	}

	object DECODER extends ScalaObjectParameter[Decoder.Factory]("", c => FullDecoder) {
		override def format(value: Decoder.Factory): String = value.name
	}

	override def isIterative: Boolean = true

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer with IterativeTrainer {

		val order = configuration(ORDER)
		val features = configuration(FEATURES)
		val iterationCount = configuration(ITERATION_COUNT)
		val useAveraging = configuration(AVERAGING)
		val decoderFactory = configuration(DECODER)

		def train(corpus: Corpus, callback: IterationCallback): Unit = {
			val breadth = corpus.tagSet.size

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

			println((wordOnlyFeatures.size + otherFeatures.size) + " feature parameters")

			val extractWeights: ((Weights, Weights)) => Weights = {
				case (weights, averagedWeights) => weights
			}
			val hmm = HMMDiscriminant(breadth, order,
				wordOnlyFeatures.map(extractWeights), otherFeatures.map(extractWeights), dictionary)

			val decoder = decoderFactory.instantiate(hmm, configuration)

			for (i <- 1 to iterationCount) {

				val (_, elapsedTime) = timed {
					val progress = new ProgressBar(f"Iteration $i%2d/$iterationCount%2d", corpus.size)
					progress.set(0)

					val refSource = new SourceTracker(breadth, order)
					val hypSource = new SourceTracker(breadth, order)

					corpus.foreach { refSeq: Sentence =>

						val hypSeq = decoder.decode(refSeq)

						if (refSeq.length != hypSeq.length) {
							throw new IllegalStateException("Observable length mismatch!")
						}

						val iterator = refSeq.zippedHistoryIterator(hypSeq)
						while (iterator.hasNext) {
							val (refHistory, hypHistory) = iterator.next()

							val (oRef, sRef) = (refHistory.current.get(Form), refHistory.current.get(CoarsePosTag))
							val (oHyp, sHyp) = (hypHistory.current.get(Form), hypHistory.current.get(CoarsePosTag))

							if (oRef != oHyp) {
								throw new IllegalStateException("Observable mismatch!")
							}

							if (sRef != sHyp || refSource.state != hypSource.state) {

								val refTagFilter = BitSet(sRef)
								val hypTagFilter = BitSet(sHyp)
								val refPreviousTags = refSource.tags
								val hypPreviousTags = hypSource.tags

								wordOnlyFeatures.foreachMatching(refHistory, refPreviousTags, refTagFilter) {
									case (weights, _) => weights(sRef) += 1.0
								}
								wordOnlyFeatures.foreachMatching(hypHistory, hypPreviousTags, hypTagFilter) {
									case (weights, _) => weights(sHyp) -= 1.0
								}

								otherFeatures.foreachMatching(refHistory, refPreviousTags, refTagFilter) {
									case (weights, _) => weights(sRef) += 1.0
								}
								otherFeatures.foreachMatching(hypHistory, hypPreviousTags, hypTagFilter) {
									case (weights, _) => weights(sHyp) -= 1.0
								}
							}

							refSource.append(sRef)
							hypSource.append(sHyp)
						}

						if (useAveraging == COMPLETE_AVERAGING) {
							allWeightPairs.par.foreach {
								case (weights, averagedWeights) =>
									weights.foreach { case (tag, weight) => averagedWeights(tag) += weight}
							}
						}

						refSource.reset()
						hypSource.reset()
						progress.increment()
					}

					if (useAveraging == PARTIAL_AVERAGING) {
						allWeightPairs.par.foreach {
							case (weights, averagedWeights) =>
								weights.foreach { case (tag, weight) => averagedWeights(tag) += weight}
						}
					}
				}

				val resultHmm =
					if (useAveraging == NO_AVERAGING) hmm
					else {
						val averagingDivider = i * (if (useAveraging == COMPLETE_AVERAGING) corpus.size else 1)
						val divideWeights: ((Weights, Weights)) => Weights = {
							case (weights, averagedWeights) => averagedWeights.map(w => w / averagingDivider)
						}
						HMMDiscriminant(breadth, order,
							wordOnlyFeatures.map(divideWeights), otherFeatures.map(divideWeights), dictionary
						)
					}
				callback.iterationDone(configuration.set(ITERATION_COUNT, i), resultHmm, elapsedTime)
			}
		}
	}

}

package io.github.ptitjes.scott.trainers

import io.github.ptitjes.scott.Features._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.decoders.FullDecoder
import io.github.ptitjes.scott.utils.States.SourceTracker

import scala.collection._

object DiscriminantTrainer {
	val NO_AVERAGING = 0
	val PARTIAL_AVERAGING = 1
	val COMPLETE_AVERAGING = 2
}

class DiscriminantTrainer[X, Y <: X](order: Int,
                                     iterationCount: Int,
                                     useAveraging: Int,
                                     features: FeatureSetTemplate[X, Y],
                                     observableExtract: X => Int,
                                     tagExtract: Y => Int,
                                     builder: (X, Int) => Y) extends Trainer[X, Y] with IterativeTrainer[X, Y] {

	import DiscriminantTrainer._

	def train(corpus: Corpus[Y], callback: IterationCallback[X, Y]): Unit = {
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
			features.buildFeatures(breadth, order, corpus, weightFactory, observableExtract, tagExtract)

		println((wordOnlyFeatures.size + otherFeatures.size) + " feature parameters")

		val extractWeights: ((Weights, Weights)) => Weights = {
			case (weights, averagedWeights) => weights
		}
		val hmm = HMMDiscriminant(breadth, order,
			wordOnlyFeatures.map(extractWeights), otherFeatures.map(extractWeights), dictionary, observableExtract, builder)

		val decoder = new FullDecoder(hmm)

		for (i <- 1 to iterationCount) {

			val (_, elapsedTime) = timed {
				val progress = new ProgressBar(f"Iteration $i%2d/$iterationCount%2d", corpus.size)
				progress.set(0)

				val refSource = new SourceTracker(breadth, order)
				val hypSource = new SourceTracker(breadth, order)

				corpus.foreach {
					refSeq =>

						val hypSeq = decoder.decode(refSeq)

						if (refSeq.length != hypSeq.length) {
							throw new IllegalStateException("Observable length mismatch!")
						}

						val iterator = refSeq.zippedHistoryIterator(hypSeq)
						while (iterator.hasNext) {
							val (refHistory, hypHistory) = iterator.next()

							val (oRef, sRef) = (observableExtract(refHistory.current), tagExtract(refHistory.current))
							val (oHyp, sHyp) = (observableExtract(hypHistory.current), tagExtract(hypHistory.current))

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
									weights.foreach {
										case (tag, weight) => averagedWeights(tag) += weight
									}
							}
						}

						refSource.reset()
						hypSource.reset()
						progress.increment()
				}

				if (useAveraging == PARTIAL_AVERAGING) {
					allWeightPairs.par.foreach {
						case (weights, averagedWeights) =>
							weights.foreach {
								case (tag, weight) => averagedWeights(tag) += weight
							}
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
						wordOnlyFeatures.map(divideWeights), otherFeatures.map(divideWeights), dictionary, observableExtract, builder
					)
				}
			callback.iterationDone(i, resultHmm, elapsedTime)
		}
	}
}

package io.github.ptitjes.scott.trainers

import io.github.ptitjes.scott.api.Features._
import io.github.ptitjes.scott.api.HiddenMarkovModel._
import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.decoders.FullDecoder
import io.github.ptitjes.scott.utils.States.SourceTracker
import io.github.ptitjes.scott.utils.Utils._

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

	import io.github.ptitjes.scott.trainers.DiscriminantTrainer._

	def train(corpus: DataSet[Y], callback: IterationCallback[X, Y]): Unit = {
		val breadth = corpus.tagSet.size

		var allWeightPairs = mutable.ArrayBuffer[(MutableWeights, MutableWeights)]()
		val weightFactory: BitSet => (MutableWeights, MutableWeights) = {
			tags =>
				val weights = new MutableWeights(tags)
				val averagedWeights = new MutableWeights(tags)
				val weightPair = (weights, averagedWeights)
				allWeightPairs += weightPair
				weightPair
		}

		val (wordOnlyFeatures, otherFeatures, dictionary) =
			features.buildFeatures(breadth, order, corpus, weightFactory, observableExtract, tagExtract)

		println((wordOnlyFeatures.size + otherFeatures.size) + " feature parameters")

		val extractWeights: ((MutableWeights, MutableWeights)) => MutableWeights = {
			case (weights, averagedWeights) => weights
		}
		val extractAverageWeights: ((MutableWeights, MutableWeights)) => MutableWeights = {
			case (weights, averagedWeights) => averagedWeights
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
							allWeightPairs.foreach {
								case (weights, averagedWeights) => averagedWeights += weights
							}
						}

						refSource.reset()
						hypSource.reset()
						progress.increment()
				}

				if (useAveraging == PARTIAL_AVERAGING) {
					allWeightPairs.foreach {
						case (weights, averagedWeights) => averagedWeights += weights
					}
				}
			}

			val resultHmm =
				if (useAveraging == NO_AVERAGING) {
					HMMDiscriminant(breadth, order,
						wordOnlyFeatures.map(extractWeights andThen (_.immutable)),
						otherFeatures.map(extractWeights andThen (_.immutable)),
						dictionary, observableExtract, builder
					)
				} else {
					val averagingDivider = i * (if (useAveraging == COMPLETE_AVERAGING) corpus.size else 1)
					HMMDiscriminant(breadth, order,
						wordOnlyFeatures.map(extractAverageWeights andThen (_.immutable) andThen (_.map(_ / averagingDivider))),
						otherFeatures.map(extractAverageWeights andThen (_.immutable) andThen (_.map(_ / averagingDivider))),
						dictionary, observableExtract, builder
					)
				}
			callback.iterationDone(i, resultHmm, elapsedTime)
		}
	}
}

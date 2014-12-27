package io.github.ptitjes.scott.decoders

import io.github.ptitjes.scott.api.HiddenMarkovModel._
import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.decoders.arrays._
import io.github.ptitjes.scott.utils.Utils._
import io.github.ptitjes.scott.utils.{DepthCounter, States}

import scala.annotation.tailrec
import scala.collection._

class FullDecoder[X, Y](hmm: HiddenMarkovModel[X, Y]) extends Decoder[X, Y] {

	val breadth = hmm.breadth
	val order = hmm.depth
	val maxStateCount = pow(breadth, order)

	val deltas = new SwappableArray[Double](maxStateCount)
	val psis = new PsiArray(maxStateCount, 300)

	val scores = Array.ofDim[Double](breadth, maxStateCount)
	val wordOnlyScores = Array.ofDim[Double](breadth)

	val allTags = BitSet() ++ (0 until breadth)

	val depth = new DepthCounter(order)

	def decode(sequence: Sequence[X]): Sequence[Y] = {
		deltas(0) = 0
		psis(0) = -1
		deltas.swap()
		psis.forward()

		var sourceTagsCount = 1
		var sourceTagsFanning = 0

		var sharedTagsCount = 1
		val sharedTagsFanning = breadth
		var sharedTags = 0 until sharedTagsCount

		var allStatesCount = sourceTagsCount * sharedTagsCount
		var allStates = 0 until allStatesCount

		val targetTagsCount = breadth
		val targetTags = 0 until targetTagsCount

		val iterator = sequence.historyIterator
		while (iterator.hasNext) {
			val history = iterator.next()
			val observable = hmm.observableExtract(history.current)

			val d = depth.current

			hmm match {
				case HMMGenerative(_, _, t, e, ue, _, _, _) =>
					val Td = t(d)
					val E = if (!hmm.isUnknown(observable)) e(observable) else ue

					targetTags.foreach { targetTag =>
						val targetScores = scores(targetTag)
						val Tj = Td(targetTag)
						val Ej = E(targetTag)

						allStates.foreach { sourceState =>
							targetScores(sourceState) = Tj(sourceState) + Ej
						}
					}

				case HMMDiscriminant(_, _, wordOnlyFeatures, otherFeatures, _, _, _) =>
					targetTags.foreach { targetTag =>
						wordOnlyScores(targetTag) = 0
					}
					wordOnlyFeatures.foreachMatching(history, IndexedSeq.empty, allTags)(weights =>
						weights.foreach { case (tag, weight) => wordOnlyScores(tag) += weight}
					)

					allStates.foreach { sourceState =>
						targetTags.foreach { targetTag =>
							scores(targetTag)(sourceState) = wordOnlyScores(targetTag)
						}

						val previousTags = States.stateToTagSeq(breadth, order, d, sourceState)
						otherFeatures.foreachMatching(history, previousTags, allTags)(weights =>
							weights.foreach { case (tag, weight) => scores(tag)(sourceState) += weight}
						)
					}
			}

			sharedTags.foreach { sharedTag =>
				targetTags.foreach { targetTag =>
					val targetScores = scores(targetTag)

					val (max, argMax) = maxArgMax(sourceTagsCount,
						sourceTag => sourceTag * sourceTagsFanning + sharedTag,
						sourceState => deltas(sourceState) + targetScores(sourceState)
					)

					val targetState = sharedTag * sharedTagsFanning + targetTag
					deltas(targetState) = max
					psis(targetState) = argMax
				}
			}

			if (d < order) {
				if (d + 1 < order) {
					sharedTagsCount = pow(breadth, d + 1)
				} else {
					sourceTagsCount = breadth
					sourceTagsFanning = pow(breadth, d)

					sharedTagsCount = pow(breadth, d)
				}
				sharedTags = 0 until sharedTagsCount

				allStatesCount = sourceTagsCount * sharedTagsCount
				allStates = 0 until allStatesCount
			}

			deltas.swap()
			psis.forward()

			depth.next()
		}

		@tailrec def reachBack(state: Int, tail: List[Int]): List[Int] = {
			val previous = psis(state)
			psis.backward()

			if (psis.isRewound) tail
			else reachBack(previous, (state % breadth) :: tail)
		}

		val (_, argMax) = maxArgMax(sharedTagsCount * targetTagsCount, t => t, state => deltas(state))
		val states = reachBack(argMax, Nil)

		psis.rewind()

		depth.reset()

		Sequence(sequence.tokens.zip(states).map { case ((token, tag)) => hmm.builder(token, tag)})
	}

	@inline def maxArgMax(count: Int, arg: Int => Int, f: Int => Double): (Double, Int) = {
		var max = Double.NegativeInfinity
		var argMax: Int = -1

		var i = 0
		while (i < count) {
			val a = arg(i)
			val delta = f(a)

			if (delta > max) {
				max = delta
				argMax = a
			}
			i += 1
		}

		(max, argMax)
	}
}

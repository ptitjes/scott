package io.github.ptitjes.scott.decoders

import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.decoders.arrays._
import io.github.ptitjes.scott.utils.{States, DepthCounter, BoundedPriorityQueue}

import scala.annotation.tailrec
import scala.collection.{mutable, _}

class BeamDecoder[X, Y](hmm: HiddenMarkovModel[X, Y], beamWidth: Int = 5) extends Decoder[X, Y] {

	val breadth = hmm.breadth
	val order = hmm.depth
	val maxStateCount = pow(breadth, order)

	val deltas = new SwappableArray[Double](maxStateCount)
	val psis = new PsiArray(maxStateCount, 300)

	val beamMaxElements = BoundedPriorityQueue[(Double, Int)](beamWidth)(
		new Ordering[(Double, Int)] {
			def compare(x: (Double, Int), y: (Double, Int)): Int =
				scala.math.Ordering.Double.compare(y._1, x._1)
		}
	)
	val beam = new mutable.BitSet(maxStateCount)

	val scores = Array.ofDim[Double](breadth, maxStateCount)
	val wordOnlyScores = Array.ofDim[Double](breadth)

	val dictionary = hmm.dictionary
	val allTags = BitSet() ++ (0 until breadth)

	val depth = new DepthCounter(order)

	def decode(sequence: Sentence[X]): Sentence[Y] = {
		beam.clear()
		beam(0) = true
		deltas(0) = 0
		psis(0) = -1
		deltas.swap()
		psis.forward()

		var sourceTagsCount = 1
		var sourceTagsFanning = 0

		var sharedTagsCount = 1
		val sharedTagsFanning = breadth
		var sharedTags = 0 until sharedTagsCount

		val targetTagsCount = breadth

		val iterator = sequence.historyIterator
		while (iterator.hasNext) {
			val history = iterator.next()
			val observable = hmm.observableExtract(history.current)

			val d = depth.current

			val tags = if (dictionary.contains(observable)) dictionary(observable) else allTags

			hmm match {
				case HMMGenerative(_, _, t, e, ue, _, _, _) =>
					val Td = t(d)
					val E = if (!hmm.isUnknown(observable)) e(observable) else ue

					tags.foreach { targetTag =>
						val targetScores = scores(targetTag)
						val Tj = Td(targetTag)
						val Ej = E(targetTag)

						beam.foreach { sourceState =>
							targetScores(sourceState) = Tj(sourceState) + Ej
						}
					}

				case HMMDiscriminant(_, _, wordOnlyFeatures, otherFeatures, _, _, _) =>
					tags.foreach { targetTag =>
						wordOnlyScores(targetTag) = 0
					}
					wordOnlyFeatures.foreachMatching(history, IndexedSeq.empty, tags)(weights =>
						weights.foreach { case (tag, weight) => if (tags(tag)) wordOnlyScores(tag) += weight}
					)

					beam.foreach { sourceState =>
						tags.foreach { targetTag =>
							scores(targetTag)(sourceState) = wordOnlyScores(targetTag)
						}

						val previousTags = States.stateToTagSeq(breadth, order, d, sourceState)
						otherFeatures.foreachMatching(history, previousTags, tags)(weights =>
							weights.foreach { case (tag, weight) => if (tags(tag)) scores(tag)(sourceState) += weight}
						)
					}
			}

			beamMaxElements.clear()
			sharedTags.foreach { sharedTag =>
				tags.foreach { targetTag =>
					val targetScores = scores(targetTag)

					val (max, argMax) = maxArgMax(sourceTagsCount, beam,
						sourceTag => sourceTag * sourceTagsFanning + sharedTag,
						sourceState => deltas(sourceState) + targetScores(sourceState)
					)

					val targetState = sharedTag * sharedTagsFanning + targetTag
					deltas(targetState) = max
					psis(targetState) = argMax

					beamMaxElements += ((max, targetState))
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
			}

			deltas.swap()
			psis.forward()

			beam.clear()
			beamMaxElements.foreach {
				case (_, sourceState) => beam(sourceState) = true
			}

			depth.next()
		}

		@tailrec def reachBack(state: Int, tail: List[Int]): List[Int] = {
			val previous = psis(state)
			psis.backward()

			if (psis.isRewound) tail
			else reachBack(previous, (state % breadth) :: tail)
		}

		val (_, argMax) = maxArgMax(sharedTagsCount * targetTagsCount, beam, t => t, state => deltas(state))
		val states = reachBack(argMax, Nil)

		psis.rewind()

		depth.reset()

		Sentence(sequence.tokens.zip(states).map { case ((token, tag)) => hmm.builder(token, tag)})
	}

	@inline def maxArgMax(count: Int, beam: mutable.BitSet,
	                      arg: Int => Int, f: Int => Double): (Double, Int) = {
		var max = Double.NegativeInfinity
		var argMax: Int = -1

		var i = 0
		while (i < count) {
			val a = arg(i)
			if (beam(a)) {
				val delta = f(a)

				if (delta >= max) {
					max = delta
					argMax = a
				}
			}
			i += 1
		}

		(max, argMax)
	}
}

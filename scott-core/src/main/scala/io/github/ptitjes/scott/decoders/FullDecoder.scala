package io.github.ptitjes.scott.decoders

import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.decoders.arrays._

import scala.annotation.tailrec
import scala.collection._

object FullDecoder extends Decoder.Factory {

	def name: String = "Full"

	override def parameters: Set[Parameter[_]] = Set(MULTI_THREADED)

	object MULTI_THREADED extends BooleanParameter("MultiThreaded", c => c(Trainer.ORDER) >= 3)

	def instantiate(hmm: HiddenMarkovModel, configuration: Configuration): Decoder = new Instance(hmm, configuration)

	private class Instance(hmm: HiddenMarkovModel, configuration: Configuration) extends Decoder {

		val multiThreaded = configuration(MULTI_THREADED)

		val breadth = hmm.breadth
		val depth = hmm.depth
		val maxStateCount = pow(breadth, depth)

		val deltas = new SwappableArray[Double](maxStateCount)
		val psis = new PsiArray(maxStateCount, 300)

		val scores = Array.ofDim[Double](breadth, maxStateCount)
		val wordOnlyScores = Array.ofDim[Double](breadth)

		val allTags = BitSet() ++ (0 until breadth)

		def decode(sequence: Sequence): Sequence with Annotation = {
			deltas(0) = 0
			psis(0) = -1
			deltas.swap()
			psis.forward()

			var sourceTagsCount = 1
			var sourceTagsFanning = 0

			var sharedTagsCount = 1
			val sharedTagsFanning = breadth
			var sharedTags = makeRange(sharedTagsCount)

			var allStatesCount = sourceTagsCount * sharedTagsCount
			var allStates = makeRange(allStatesCount)

			val targetTagsCount = breadth
			val targetTags = makeRange(targetTagsCount)

			val iterator = sequence.iterator(breadth, depth)
			while (iterator.hasNext) {
				val word = iterator.next()
				val d = iterator.currentDepth

				hmm match {
					case HMMGenerative(_, _, t, e, ue, _) =>
						val Td = t(d)
						val E = if (!hmm.isUnknown(word)) e(word.code) else ue

						targetTags.foreach { targetTag =>
							val targetScores = scores(targetTag)
							val Tj = Td(targetTag)
							val Ej = E(targetTag)

							allStates.foreach { sourceState =>
								targetScores(sourceState) = Tj(sourceState) + Ej
							}
						}

					case HMMDiscriminant(_, _, wordOnlyFeatures, otherFeatures, _) =>
						targetTags.foreach { targetTag =>
							wordOnlyScores(targetTag) = 0
						}
						val h_wordOnly = iterator.history(-1)
						wordOnlyFeatures.foreachMatching(h_wordOnly, allTags)(weights =>
							weights.foreach { case (tag, weight) => wordOnlyScores(tag) += weight}
						)

						allStates.foreach { sourceState =>
							targetTags.foreach { targetTag =>
								scores(targetTag)(sourceState) = wordOnlyScores(targetTag)
							}

							val h = iterator.history(sourceState)
							otherFeatures.foreachMatching(h, allTags)(weights =>
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

				if (d < depth) {
					if (d + 1 < depth) {
						sharedTagsCount = pow(breadth, d + 1)
					} else {
						sourceTagsCount = breadth
						sourceTagsFanning = pow(breadth, d)

						sharedTagsCount = pow(breadth, d)
					}
					sharedTags = makeRange(sharedTagsCount)

					allStatesCount = sourceTagsCount * sharedTagsCount
					allStates = makeRange(allStatesCount)
				}

				deltas.swap()
				psis.forward()
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

			AnnotatedSequence(sequence.observables.zip(states))
		}

		def makeRange(count: Int): GenSeq[Int] = {
			if (multiThreaded) (0 until count).par else 0 until count
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

}
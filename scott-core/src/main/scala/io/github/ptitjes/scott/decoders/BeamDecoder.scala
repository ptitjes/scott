package io.github.ptitjes.scott.decoders

import io.github.ptitjes.scott.Corpora._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.decoders.arrays._
import io.github.ptitjes.scott.utils.BoundedPriorityQueue

import scala.annotation.tailrec
import scala.collection.{mutable, _}

object BeamDecoder extends Decoder.Factory {

	def name: String = "Beam"

	override def parameters: Set[Parameter[_]] = Set(BEAM)

	object BEAM extends IntParameter("Beam", 5)

	def instantiate(hmm: HiddenMarkovModel, configuration: Configuration): Decoder = new Instance(hmm, configuration)

	private class Instance(hmm: HiddenMarkovModel, configuration: Configuration) extends Decoder {

		val breadth = hmm.breadth
		val depth = hmm.depth
		val maxStateCount = pow(breadth, depth)

		val deltas = new SwappableArray[Double](maxStateCount)
		val psis = new PsiArray(maxStateCount, 300)

		val beamWidth = configuration(BEAM)
		val beamMaxElements = BoundedPriorityQueue[(Double, Int)](beamWidth)(
			new Ordering[(Double, Int)] {
				def compare(x: (Double, Int), y: (Double, Int)): Int =
					scala.math.Ordering.Double.compare(y._1, x._1)
			}
		)
		val beam = new mutable.BitSet(maxStateCount)

		val scores = Array.ofDim[Double](breadth, maxStateCount)
		val wordOnlyScores = Array.ofDim[Double](breadth)

		val dictionary = hmm match {
			case HMMGenerative(_, _, _, _, _, dict) => dict
			case HMMDiscriminant(_, _, _, _, dict) => dict
		}
		val allTags = BitSet() ++ (0 until breadth)

		def decode(sequence: Sequence): Sequence with Annotation = {
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

			val iterator = sequence.iterator(breadth, depth)
			while (iterator.hasNext) {
				val word = iterator.next()
				val d = iterator.currentDepth

				val tags = if (dictionary.contains(word.code)) dictionary(word.code) else allTags

				hmm match {
					case HMMGenerative(_, _, t, e, ue, _) =>
						val Td = t(d)
						val E = if (!hmm.isUnknown(word)) e(word.code) else ue

						tags.foreach { targetTag =>
							val targetScores = scores(targetTag)
							val Tj = Td(targetTag)
							val Ej = E(targetTag)

							beam.foreach { sourceState =>
								targetScores(sourceState) = Tj(sourceState) + Ej
							}
						}

					case HMMDiscriminant(_, _, wordOnlyFeatures, otherFeatures, _) =>
						tags.foreach { targetTag =>
							wordOnlyScores(targetTag) = 0
						}
						val h_wordOnly = iterator.history(-1)
						wordOnlyFeatures.foreachMatching(h_wordOnly, tags)(weights =>
							weights.foreach { case (tag, weight) => if (tags(tag)) wordOnlyScores(tag) += weight}
						)

						beam.foreach { sourceState =>
							tags.foreach { targetTag =>
								scores(targetTag)(sourceState) = wordOnlyScores(targetTag)
							}

							val h = iterator.history(sourceState)
							otherFeatures.foreachMatching(h, tags)(weights =>
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

				if (d < depth) {
					if (d + 1 < depth) {
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

			AnnotatedSequence(sequence.observables.zip(states))
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

}
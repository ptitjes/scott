package io.github.ptitjes.hmm.decoders

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.utils.BoundedPriorityQueue

import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.collection.mutable
import scala.reflect.ClassTag

object BeamDecoder extends Decoder.Factory {

	def name: String = "Beam"

	override def parameters: Set[Parameter[_]] = Set(BEAM, MULTI_THREADED)

	object MULTI_THREADED extends BooleanParameter("MultiThreaded", c => c(Trainer.ORDER) >= 3)

	object BEAM extends IntParameter("Beam", 3)

	def instantiate(hmm: HiddenMarkovModel, configuration: Configuration): Decoder = new Instance(hmm, configuration)

	private class Instance(hmm: HiddenMarkovModel, configuration: Configuration) extends Decoder {

		val multiThreaded = configuration(MULTI_THREADED)

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
					case HMMGenerative(_, _, t, e, ue) =>
						val Td = t(d)
						val E = if (!hmm.isUnknown(word)) e(word.code) else ue

						targetTags.foreach { targetTag =>
							val targetScores = scores(targetTag)
							val Tj = Td(targetTag)
							val Ej = E(targetTag)

							beam.foreach { sourceState =>
								targetScores(sourceState) = Tj(sourceState) + Ej
							}
						}

					case HMMDiscriminant(_, _, wordOnlyFeatures, otherFeatures, dictionary) =>
						targetTags.foreach { targetTag =>
							wordOnlyScores(targetTag) = 0
						}

						val h_wordOnly = iterator.history(-1)
						wordOnlyFeatures.foreachMatching(h_wordOnly)(weights =>
							weights.foreach { case (tag, weight) => wordOnlyScores(tag) += weight}
						)
						beam.foreach { sourceState =>
							targetTags.foreach { targetTag =>
								scores(targetTag)(sourceState) = wordOnlyScores(targetTag)
							}

							val h = iterator.history(sourceState)
							otherFeatures.foreachMatching(h)(weights =>
								weights.foreach { case (tag, weight) => scores(tag)(sourceState) += weight}
							)
						}
				}

				beamMaxElements.clear()
				sharedTags.foreach { sharedTag =>
					targetTags.foreach { targetTag =>
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
					sharedTags = makeRange(sharedTagsCount)

					allStatesCount = sourceTagsCount * sharedTagsCount
					allStates = makeRange(allStatesCount)
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

		def makeRange(count: Int): GenSeq[Int] = {
			if (multiThreaded) (0 until count).par else 0 until count
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

		final class SwappableArray[T: ClassTag](size: Int) {
			private var current: Array[T] = new Array[T](size)
			private var last: Array[T] = new Array[T](size)

			def apply(i: Int): T = last(i)

			def update(i: Int, v: T) = current(i) = v

			def swap(): Unit = {
				val temp = current
				current = last
				last = temp
			}
		}

		final class PsiArray(size: Int, length: Int) {
			private val data = Array.ofDim[Int](length, size)
			private var index = 0

			def apply(i: Int): Int = data(index - 1)(i)

			def update(i: Int, v: Int) = data(index)(i) = v

			def forward(): Unit = index = index + 1

			def backward(): Unit = index = index - 1

			def isRewound: Boolean = index == 0

			def rewind(): Unit = index = 0
		}

	}

}
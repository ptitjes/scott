package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._

import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.reflect.ClassTag

object BeamDecoder extends Algorithm[Decoder] {

	def name: String = "Full"

	override def parameters: Set[Parameter[_]] = Set(BEAM, MULTI_THREADED)

	object MULTI_THREADED extends BooleanParameter("MultiThreaded", c => c(Trainer.ORDER) >= 3)

	object BEAM extends IntParameter("Beam", 3)

	def instantiate(configuration: Configuration): Decoder = new Instance(configuration)

	private class Instance(configuration: Configuration) extends Decoder {

		import io.github.ptitjes.hmm.Corpora._
		import io.github.ptitjes.hmm.Utils._

		val multiThreaded = configuration(MULTI_THREADED)

		var hmm: HiddenMarkovModel = null
		var breadth = 0
		var depth = 0

		var deltas: SwappableArray[Double] = null
		var psis: PsiArray = null

		var beamSize: Double = 1
		var beam: Array[Boolean] = null

		var scores: Array[Array[Double]] = null
		var wordOnlyScores: Array[Double] = null

		def setHmm(hmm: HiddenMarkovModel): Unit = {
			this.hmm = hmm
			this.breadth = hmm.breadth
			this.depth = hmm.depth

			val maxStateCount = pow(breadth, depth)
			deltas = new SwappableArray[Double](maxStateCount)
			psis = new PsiArray(maxStateCount, 300)

			beam = Array.ofDim(maxStateCount)
			beamSize = 1.0 / (1 + configuration(BEAM))

			scores = Array.ofDim[Double](breadth, maxStateCount)
			wordOnlyScores = Array.ofDim[Double](breadth)
		}

		def decode(sequence: Sequence): Sequence with Annotation = {
			for (i <- 0 until pow(breadth, depth))
				beam(i) = true

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

							allStates.foreach { sourceState =>
								if (beam(sourceState)) {
									targetScores(sourceState) = Tj(sourceState) + Ej
								}
							}
						}

					case HMMDiscriminant(_, _, wordOnlyFeatures, otherFeatures, dictionary) =>
						targetTags.foreach { targetTag =>
							wordOnlyScores(targetTag) = 0
						}

						val h_wordOnly = iterator.history(-1)
						wordOnlyFeatures.foreach(h_wordOnly)(weights =>
							targetTags.foreach { targetTag =>
								wordOnlyScores(targetTag) += weights(targetTag)
							})
						allStates.foreach { sourceState =>
							if (beam(sourceState)) {
								targetTags.foreach { targetTag =>
									scores(targetTag)(sourceState) = wordOnlyScores(targetTag)
								}
							}
						}

						allStates.foreach { sourceState =>
							if (beam(sourceState)) {
								val h = iterator.history(sourceState)
								otherFeatures.foreach(h)(weights =>
									targetTags.foreach { targetTag =>
										scores(targetTag)(sourceState) += weights(targetTag)
									})
							}
						}
				}

				var beamMax = -1.0e307
				var beamMin = 1.0e307
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

						if (max > beamMax) beamMax = max
						if (max < beamMin) beamMin = max
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

				beamMin = if (beamMin.isNegInfinity) -1.0e307 else beamMin
				beamMin = beamMax - math.pow(math.abs(beamMax - beamMin), beamSize)
				allStates.foreach { sourceState =>
					beam(sourceState) = deltas(sourceState) >= beamMin
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

		@inline def maxArgMax(count: Int, beam: Array[Boolean],
		                      arg: Int => Int, f: Int => Double): (Double, Int) = {
			var max = Double.NegativeInfinity
			var argMax: Int = -1

			var i = 0
			while (i < count) {
				val a = arg(i)
				if (beam(a)) {
					val delta = f(a)

					if (delta > max) {
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
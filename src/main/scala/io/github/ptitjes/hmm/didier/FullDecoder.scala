package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Features.WordComponents
import io.github.ptitjes.hmm._

import scala.annotation.tailrec
import scala.collection.{mutable, GenSeq}
import scala.reflect.ClassTag

object FullDecoder extends Algorithm[Decoder] {

	def name: String = "Full"

	override def parameters: Set[Parameter[_]] = Set(MULTI_THREADED)

	object MULTI_THREADED extends BooleanParameter("MultiThreaded", c => c(Trainer.ORDER) >= 3)

	def instantiate(configuration: Configuration): Decoder = new Instance(configuration)

	private class Instance(configuration: Configuration) extends Decoder {

		import io.github.ptitjes.hmm.Corpora._
		import io.github.ptitjes.hmm.Utils._

		val multiThreaded = configuration(MULTI_THREADED)

		var hmm: HiddenMarkovModel = null
		var breadth = 0
		var depth = 0
		var noFeatures = false

		var deltas: SwappableArray[Double] = null
		var psis: PsiArray = null

		def setHmm(hmm: HiddenMarkovModel): Unit = {
			this.hmm = hmm
			this.breadth = hmm.breadth
			this.depth = hmm.depth
			this.noFeatures = hmm.UE match {
				case UEPShared(ue) => true
				case _ => false
			}

			val maxStateCount = pow(breadth, depth)
			deltas = new SwappableArray[Double](maxStateCount)
			psis = new PsiArray(maxStateCount, 300)
		}

		def decode(sequence: Sequence): Sequence with Annotation = {
			deltas(0) = 0
			psis(0) = -1
			deltas.swap()
			psis.forward()

			var d = 0
			var T = hmm.T(d)

			var sourceTagsCount = 1
			var sourceTagsFanning = 0

			var sharedTagsCount = 1
			val sharedTagsFanning = breadth
			var sharedTags = makeRange(sharedTagsCount)

			val targetTagsCount = breadth
			val targetTags = makeRange(targetTagsCount)

			sequence.observables.foreach { o =>
				val unknown = hmm.isUnknown(o)
				if (!unknown || noFeatures) {
					val E = if (!unknown) hmm.E(o) else hmm.UE.asInstanceOf[UEPShared].ue

					sharedTags.foreach { sharedTag =>
						targetTags.foreach { targetTag =>
							val Tj = T(targetTag)
							val Ej = E(targetTag)

							val (max, argMax) = maxArgMax(sourceTagsCount,
								sourceTag => sourceTag * sourceTagsFanning + sharedTag,
								sourceState => deltas(sourceState) + Tj(sourceState) + Ej
							)

							val targetState = sharedTag * sharedTagsFanning + targetTag
							deltas(targetState) = max
							psis(targetState) = argMax
						}
					}
				} else {
					val UE = hmm.UE match {
						case UEPFeatureBased(features) =>
							val word = WordComponents(Lexica.WORDS(o))
							val score = Array.ofDim[Double](targetTagsCount, sourceTagsCount * sharedTagsCount)

							sharedTags.foreach { sharedTag =>
								(0 until sourceTagsCount).foreach { sourceTag =>
									val sourceState = sourceTag * sourceTagsFanning + sharedTag
									val h_1 = if (d == 0) -1 else sourceState % breadth
									val h_2 = if (d <= 1) -1 else sourceState / breadth % breadth
									features.foreach { case (f, weights) =>
										if (f(h_2, h_1, word)) {
											targetTags.foreach { targetTag =>
												score(targetTag)(sourceState) += weights(targetTag)
											}
										}
									}
								}
							}
							score
						case _ => throw new IllegalStateException()
					}

					sharedTags.foreach { sharedTag =>
						targetTags.foreach { targetTag =>
							val UEj = UE(targetTag)
							val Tj = T(targetTag)

							val (max, argMax) = maxArgMax(sourceTagsCount,
								sourceTag => sourceTag * sourceTagsFanning + sharedTag,
								sourceState => deltas(sourceState) + Tj(sourceState) + UEj(sourceState)
							)

							val targetState = sharedTag * sharedTagsFanning + targetTag
							deltas(targetState) = max
							psis(targetState) = argMax
						}
					}
				}

				if (d < depth) {
					d += 1
					T = hmm.T(d)

					if (d < depth) {
						sharedTagsCount = pow(breadth, d)
					} else {
						sourceTagsCount = breadth
						sourceTagsFanning = pow(breadth, d - 1)

						sharedTagsCount = pow(breadth, d - 1)
					}
					sharedTags = makeRange(sharedTagsCount)
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
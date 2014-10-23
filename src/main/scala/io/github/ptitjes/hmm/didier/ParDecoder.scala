package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object ParDecoder extends Algorithm[Decoder] {

  def name: String = "ArrayMT"

  override def parameters: Set[Parameter[_]] = Set()

  def instantiate(configuration: Configuration): Decoder = new Instance(configuration)

  class Instance(configuration: Configuration) extends Decoder {

    import io.github.ptitjes.hmm.Corpora._
    import io.github.ptitjes.hmm.Utils._

    def decode(hmm: HiddenMarkovModel,
               corpus: Corpus[Sequence]): Corpus[Sequence with Annotation] = {

      val deltas = new SwappableArray[Double](pow(hmm.breadth, hmm.depth))
      val psis = new PsiArray(pow(hmm.breadth, hmm.depth), 150)

      corpus.map { sequence =>
        deltas(0) = 0
        psis(0) = -1
        deltas.swap()
        psis.forward()

        var d = 0
        var T = hmm.T(d)
        var sourceCount = 1
        var targetCount = hmm.breadth
        var targets = (0 until targetCount).par

        sequence.observables.foreach { o =>
          val E = hmm.E(o)

          targets.foreach { s =>
            val x = s / hmm.breadth
            val j = s % hmm.breadth
            val Tj = T(j)
            val Ej = E(j)

            val (max, argMax) = maxArgMax(sourceCount,
              i => x + (if (sourceCount == 1) 0 else i * targetCount / sourceCount),
              a => deltas(a) + Tj(a) + Ej
            )

            deltas(s) = max
            psis(s) = argMax
          }

          if (d < hmm.depth) {
            d += 1
            T = hmm.T(d)

            if (d == hmm.depth) {
              sourceCount = hmm.breadth
              targetCount = pow(hmm.breadth, hmm.depth)
            } else {
              targetCount = pow(hmm.breadth, d + 1)
            }
            targets = (0 until targetCount).par
          }

          deltas.swap()
          psis.forward()
        }

        @tailrec def reachBack(i: Int, tail: List[Int]): List[Int] = {
          val previous = psis(i)
          psis.backward()

          if (psis.isRewound) tail
          else reachBack(previous, (i % hmm.breadth) :: tail)
        }

        val (_, argMax) = maxArgMax(targetCount, i => i, i => deltas(i))
        val states = reachBack(argMax, Nil)

        psis.rewind()

        AnnotatedSequence(sequence.observables, states.toArray)
      }
    }

    def maxArgMax(count: Int, arg: Int => Int, f: Int => Double): (Double, Int) = {
      var max = Double.NegativeInfinity
      var argMax: Int = -1

      var i = 0
      while (i < count) {
        val a = arg(i)
        val delta = f(a)

        if (delta >= max) {
          max = delta
          argMax = a
        }
        i += 1
      }

      (max, argMax)
    }

    class SwappableArray[T: ClassTag](size: Int) {
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

    class PsiArray(size: Int, length: Int) {
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
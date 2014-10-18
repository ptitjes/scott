package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object STImplementations extends Algorithms {

  import Corpora._
  import Utils._

  def trainWithRelativeFrequence(breadth: Int, depth: Int,
                                 train: Corpus[Sequence with Annotation],
                                 dev: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
    val size = pow(breadth, depth)

    val allInitialCategoryCounts = MatrixTree[Int](breadth, depth)
    val perInitialCategoryCounts = MatrixTree[Int](breadth, depth)

    val allWordCategoryCounts = Array.ofDim[Int](breadth)
    var perWordCategoryCounts: Map[Int, Array[Int]] = Map()
    val unknownWordCategoryCounts: Array[Int] = Array.ofDim(breadth)

    train.sequences.foreach { s: Sequence with Annotation =>
      var d = 0
      var previousState = 0

      s.observablesAndStates.foreach { case (word, cat) =>

        perInitialCategoryCounts(d)(cat)(previousState) += 1
        allInitialCategoryCounts(d)(0)(previousState) += 1

        if (d < depth) {
          d += 1
        }
        previousState = previousState * breadth + cat
        previousState = previousState % size

        // Emission counts
        if (!perWordCategoryCounts.contains(word)) {
          perWordCategoryCounts += word -> Array.ofDim(breadth)
        }
        perWordCategoryCounts(word)(cat) += 1
        allWordCategoryCounts(cat) += 1
      }
    }

    dev.sequences.foreach { s: Sequence with Annotation =>
      s.observablesAndStates.foreach { case (word, cat) =>

        if (!perWordCategoryCounts.contains(word)) {
          unknownWordCategoryCounts(cat) += 1
          allWordCategoryCounts(cat) += 1
        }
      }
    }

    val T = MatrixTree[Double](breadth, depth)
    var E: Map[Int, Array[Double]] = Map()
    val UE: Array[Double] = Array.ofDim(breadth)

    for (d <- 0 to depth) {
      for (i <- 0 until pow(breadth, d)) {
        for (j <- 0 until breadth) {
          T(d)(j)(i) = log(perInitialCategoryCounts(d)(j)(i)) - log(allInitialCategoryCounts(d)(0)(i))
        }
      }
    }

    perWordCategoryCounts.foreach {
      case (o, wordCategoryCounts) =>
        val emitProbabilities: Array[Double] = Array.ofDim(breadth)
        for (j <- 0 until breadth) {
          emitProbabilities(j) = log(wordCategoryCounts(j)) - log(allWordCategoryCounts(j))
        }
        E += o -> emitProbabilities
    }

    for (j <- 0 until breadth) {
      UE(j) = log(unknownWordCategoryCounts(j)) - log(allWordCategoryCounts(j))
    }

    HiddenMarkovModel(breadth, depth, T, E, UE)
  }

  def trainWithPerceptron(breadth: Int, depth: Int,
                          train: Corpus[Sequence with Annotation],
                          dev: Corpus[Sequence with Annotation]): HiddenMarkovModel = ???

  def mostProbableSequences(hmm: HiddenMarkovModel,
                            test: Corpus[Sequence]): Corpus[Sequence with Annotation] = {

    val deltas = new SwappableArray[Double](pow(hmm.breadth, hmm.depth))
    val psis = new PsiArray(pow(hmm.breadth, hmm.depth), 150)

    test.map { sequence =>
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
          val j = s % hmm.breadth
          val Tj = T(j)
          val Ej = E(j)

          val (max, argMax) = maxArgMax(0, sourceCount,
            i => deltas(i) + Tj(i) + Ej
          )
          deltas(s) = max
          psis(s) = argMax
        }

        sourceCount = targetCount
        if (d < hmm.depth) {
          d += 1
          T = hmm.T(d)
          targetCount = pow(hmm.breadth, d)
          targets = (0 until targetCount).par
        }

        deltas.swap()
        psis.forward()
      }

      @tailrec def reachBack(i: Int, tail: List[Int]): List[Int] = {
        val previous = psis(i)
        if (previous == -1) tail
        else {
          psis.backward()
          reachBack(previous, (i % hmm.breadth) :: tail)
        }
      }

      val (_, argMax) = maxArgMax(0, sourceCount, i => deltas(i))
      val states = reachBack(argMax, Nil)

      psis.rewind()

      AnnotatedSequence(sequence.observables, states.toArray)
    }
  }

  def maxArgMax(start: Int, end: Int, f: Int => Double): (Double, Int) = {
    var max = Double.NegativeInfinity
    var argMax: Int = -1

    var i = start
    while (i < end) {
      val delta = f(i)
      if (delta >= max) {
        max = delta
        argMax = i
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

    def rewind(): Unit = index = 0
  }

}

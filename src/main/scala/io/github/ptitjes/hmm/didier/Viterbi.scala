package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Corpora.{CorpusItem, SentenceSeparator, WordCategoryPair}
import io.github.ptitjes.hmm.{MatrixTree, HiddenMarkovModel}

import scala.reflect.ClassTag

object Viterbi {

  def mostProbableStateSequence(observables: Iterator[Int], hmm: HiddenMarkovModel): List[Int] = {
    var deltas = SwappableArray[Double](pow(hmm.breadth, hmm.depth))
    var psis = PsiArray(pow(hmm.breadth, hmm.depth), 150)

    deltas(0) = 0
    psis(0) = -1
    deltas = deltas.swap()
    psis = psis.forward()

    var d = 0
    observables.foreach { o =>
      val E = hmm.E(o)

      if (d < hmm.depth) {
        val PI = hmm.PI(d)

        for (j <- 0 until pow(hmm.breadth, d + 1)) {
          val (max, argMax) = maxArgMax(0 until pow(hmm.breadth, d),
            i => deltas(i) + PI(i)(j) + E(j % hmm.breadth)
          )
          deltas(j) = max
          psis(j) = argMax
        }
        d += 1
      } else {
        val T = hmm.T

        for (j <- 0 until pow(hmm.breadth, hmm.depth)) {
          val (max, argMax) = maxArgMax(0 until pow(hmm.breadth, hmm.depth),
            i => deltas(i) + T(i)(j) + E(j % hmm.breadth)
          )
          deltas(j) = max
          psis(j) = argMax
        }
      }
      deltas = deltas.swap()
      psis = psis.forward()
    }

    def reachBack(psis: PsiArray, row: Int, tail: List[Int]): List[Int] = {
      val previous = psis(row)
      if (previous == -1) tail
      else reachBack(psis.backward(), previous, (row % hmm.breadth) :: tail)
    }

    if (d < hmm.depth) {
      val (_, argMax) = maxArgMax(0 until pow(hmm.breadth, d), i => deltas(i))
      reachBack(psis, argMax, Nil)
    } else {
      val (_, argMax) = maxArgMax(0 until pow(hmm.breadth, hmm.depth), i => deltas(i))
      reachBack(psis, argMax, Nil)
    }
  }

  def pow(a: Int, b: Int) = math.pow(a, b).asInstanceOf[Int]

  def maxArgMax(range: Range, f: Int => Double): (Double, Int) = {
    var max = Double.NegativeInfinity
    var argMax: Int = -1
    for (i <- range) {
      val delta = f(i)
      if (delta >= max) {
        max = delta
        argMax = i
      }
    }
    (max, argMax)
  }

  case class SwappableArray[T](current: Array[T], last: Array[T]) {
    def apply(i: Int): T = last(i)

    def update(i: Int, v: T) = current(i) = v

    def swap(): SwappableArray[T] = SwappableArray(last, current)
  }

  object SwappableArray {
    def apply[T: ClassTag](size: Int): SwappableArray[T] =
      SwappableArray[T](new Array[T](size), new Array[T](size))
  }

  case class PsiArray(data: Array[Array[Int]], index: Int) {
    def apply(i: Int): Int = data(index - 1)(i)

    def update(i: Int, v: Int) = data(index)(i) = v

    def forward(): PsiArray = PsiArray(data, index + 1)

    def backward(): PsiArray = PsiArray(data, index - 1)
  }

  object PsiArray {
    def apply(size: Int, length: Int): PsiArray =
      PsiArray(Array.ofDim[Int](length, size), 0)
  }

  class StateTrail(breadth: Int, depth: Int) {
    private val trail: Array[Int] = Array.ofDim(depth)
    private var nextOffset: Int = 0
    private var _length: Int = 0

    def stateIndex = {
      var value: Int = 0
      for (i <- nextOffset - _length to nextOffset - 1) {
        value = value * breadth + trail(i % depth)
      }
      value
    }

    def push(state: Int): (Int, Int) = {
      val oldIndex = stateIndex

      trail(nextOffset % depth) = state
      nextOffset += 1
      if (_length < depth) {
        _length += 1
      }

      (oldIndex, stateIndex)
    }

    def peek(): Int = trail(nextOffset)

    def length: Int = _length

    def clear(): Unit = {
      nextOffset = 0
      _length = 0
    }
  }

  def trainHMM(breadth: Int, depth: Int, trainCorpus: Iterator[CorpusItem], devCorpus: Iterator[CorpusItem]): HiddenMarkovModel = {

    val allInitialCategoryCounts = MatrixTree[Int](breadth, depth)
    val perInitialCategoryCounts = MatrixTree[Int](breadth, depth)

    val allCategoryCounts = Array.ofDim[Int](pow(breadth, depth))
    val perCategoryCounts = Array.ofDim[Int](pow(breadth, depth), pow(breadth, depth))

    val allWordCategoryCounts = Array.ofDim[Int](breadth)
    var perWordCategoryCounts: Map[Int, Array[Int]] = Map()
    var allUnknownWordCategoryCounts: Int = 0
    val perUnknownWordCategoryCounts: Array[Int] = Array.ofDim(pow(breadth, depth))

    val trail = new StateTrail(breadth, depth)
    trainCorpus.foreach {
      case WordCategoryPair(word, cat) =>

        val trailLength = trail.length
        if (trailLength < depth) {
          val (i, j) = trail.push(cat)
          perInitialCategoryCounts(trailLength)(i)(j) += 1
          allInitialCategoryCounts(trailLength)(i)(0) += 1
        } else {
          val (i, j) = trail.push(cat)
          perCategoryCounts(i)(j) += 1
          allCategoryCounts(i) += 1
        }

        // Emission counts
        if (!perWordCategoryCounts.contains(word)) {
          perWordCategoryCounts += word -> Array.ofDim(breadth)
        }
        perWordCategoryCounts(word)(cat) += 1
        allWordCategoryCounts(cat) += 1
      case SentenceSeparator =>
        trail.clear()
    }

    devCorpus.foreach {
      case WordCategoryPair(word, cat) =>
        if (!perWordCategoryCounts.contains(word)) {
          perUnknownWordCategoryCounts(cat) += 1
          allUnknownWordCategoryCounts += 1
        }
      case SentenceSeparator =>
    }

    val PI = MatrixTree[Double](breadth, depth)
    val T: Array[Array[Double]] = Array.ofDim(pow(breadth, depth), pow(breadth, depth))
    var E: Map[Int, Array[Double]] = Map()
    val UE: Array[Double] = Array.ofDim(pow(breadth, depth))

    for (d <- 0 until depth) {
      val pis = PI(d)

      for (i <- 0 until pow(breadth, d)) {
        for (j <- 0 until pow(breadth, d + 1)) {
          pis(i)(j) = Math.log(perInitialCategoryCounts(d)(i)(j).toDouble) - Math.log(allInitialCategoryCounts(d)(i)(0).toDouble)
        }
      }
    }

    for (i <- 0 until pow(breadth, depth)) {
      for (j <- 0 until pow(breadth, depth)) {
        T(i)(j) = Math.log(perCategoryCounts(i)(j).toDouble) - Math.log(allCategoryCounts(i).toDouble)
      }

      UE(i) = Math.log(perUnknownWordCategoryCounts(i).toDouble) - Math.log(allUnknownWordCategoryCounts.toDouble)
    }

    perWordCategoryCounts.foreach {
      case (o, wordCategoryCounts) =>
        val emitProbabilities: Array[Double] = Array.ofDim(breadth)
        for (i <- 0 until breadth) {
          emitProbabilities(i) = Math.log(wordCategoryCounts(i).toDouble) - Math.log(allWordCategoryCounts(i).toDouble)
        }
        E += o -> emitProbabilities
    }

    HiddenMarkovModel(breadth, depth, PI, T, E, UE)
  }

}

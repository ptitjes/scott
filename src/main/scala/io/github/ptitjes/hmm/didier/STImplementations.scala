package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.{Algorithms, MatrixTree, HiddenMarkovModel}

import scala.annotation.tailrec
import scala.reflect.ClassTag

object STImplementations extends Algorithms {

  def trainWithRelativeFrequence(breadth: Int, depth: Int,
                                 train: Corpus[Sequence with Annotation],
                                 dev: Corpus[Sequence with Annotation]): HiddenMarkovModel = {

    val allInitialCategoryCounts = MatrixTree[Int](breadth, depth)
    val perInitialCategoryCounts = MatrixTree[Int](breadth, depth)

    val allCategoryCounts = Array.ofDim[Int](pow(breadth, depth))
    val perCategoryCounts = Array.ofDim[Int](pow(breadth, depth), pow(breadth, depth))

    val allWordCategoryCounts = Array.ofDim[Int](breadth)
    var perWordCategoryCounts: Map[Int, Array[Int]] = Map()
    var allUnknownWordCategoryCounts: Int = 0
    val perUnknownWordCategoryCounts: Array[Int] = Array.ofDim(pow(breadth, depth))

    val trail = new StateTrail(breadth, depth)
    train.sequences.foreach { s: Sequence with Annotation =>
      s.observablesAndStates.foreach { case (word, cat) =>

        val trailLength = trail.length
        if (trailLength < depth) {
          val (i, j) = trail.push(cat)
          perInitialCategoryCounts(trailLength)(j)(i) += 1
          allInitialCategoryCounts(trailLength)(0)(i) += 1
        } else {
          val (i, j) = trail.push(cat)
          perCategoryCounts(j)(i) += 1
          allCategoryCounts(i) += 1
        }

        // Emission counts
        if (!perWordCategoryCounts.contains(word)) {
          perWordCategoryCounts += word -> Array.ofDim(breadth)
        }
        perWordCategoryCounts(word)(cat) += 1
        allWordCategoryCounts(cat) += 1
      }
      trail.clear()
    }

    dev.sequences.foreach { s: Sequence with Annotation =>
      s.observablesAndStates.foreach { case (word, cat) =>
        if (!perWordCategoryCounts.contains(word)) {
          perUnknownWordCategoryCounts(cat) += 1
          allUnknownWordCategoryCounts += 1
        }
      }
    }

    val PI = MatrixTree[Double](breadth, depth)
    val T: Array[Array[Double]] = Array.ofDim(pow(breadth, depth), pow(breadth, depth))
    var E: Map[Int, Array[Double]] = Map()
    val UE: Array[Double] = Array.ofDim(pow(breadth, depth))

    for (d <- 0 until depth) {
      val pis = PI(d)

      for (i <- 0 until pow(breadth, d)) {
        for (j <- 0 until pow(breadth, d + 1)) {
          pis(j)(i) = Math.log(perInitialCategoryCounts(d)(j)(i).toDouble) - Math.log(allInitialCategoryCounts(d)(0)(i).toDouble)
        }
      }
    }

    for (i <- 0 until pow(breadth, depth)) {
      for (j <- 0 until pow(breadth, depth)) {
        T(j)(i) = Math.log(perCategoryCounts(j)(i).toDouble) - Math.log(allCategoryCounts(i).toDouble)
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

  def trainWithPerceptron(breadth: Int, depth: Int,
                          train: Corpus[Sequence with Annotation],
                          dev: Corpus[Sequence with Annotation]): HiddenMarkovModel = ???

  def mostProbableStateSequence(hmm: HiddenMarkovModel,
                                sequence: Sequence): Sequence with Annotation = {

    val size = pow(hmm.breadth, hmm.depth)
    val deltas = new SwappableArray[Double](size)
    val psis = new PsiArray(size, 150)

    deltas(0) = 0
    psis(0) = -1
    deltas.swap()
    psis.forward()

    var d = 0
    sequence.observables.foreach { o =>
      val E = hmm.E(o)

      if (d < hmm.depth) {
        val PI = hmm.PI(d)

        var j = 0
        while (j < pow(hmm.breadth, d + 1)) {
          val PIj = PI(j)
          val s = j % hmm.breadth

          val (max, argMax) = maxArgMax(0, pow(hmm.breadth, d),
            i => deltas(i) + PIj(i) + E(s)
          )
          deltas(j) = max
          psis(j) = argMax
          j += 1
        }
        d += 1
      } else {

        var j = 0
        while (j < size) {
          val Tj = hmm.T(j)
          val s = j % hmm.breadth

          val (max, argMax) = maxArgMax(0, size,
            i => deltas(i) + Tj(i) + E(s)
          )
          deltas(j) = max
          psis(j) = argMax
          j += 1
        }
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

    val (_, argMax) =
      if (d < hmm.depth) maxArgMax(0, pow(hmm.breadth, d), i => deltas(i))
      else maxArgMax(0, size, i => deltas(i))
    val states = reachBack(argMax, Nil)

    AnnotatedSequence(sequence.observables, states.toArray)
  }

  def pow(a: Int, b: Int) = math.pow(a, b).asInstanceOf[Int]

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

}
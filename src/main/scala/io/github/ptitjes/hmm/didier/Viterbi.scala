package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Corpora.{CorpusItem, SentenceSeparator, WordCategoryPair}
import io.github.ptitjes.hmm.HiddenMarkovModel

object Viterbi {

  def mostProbableStateSequence(observables: Iterator[Int], hmm: HiddenMarkovModel): StateSeq = {
    var deltas = Swappable(new Array[Double](hmm.nbe), new Array[Double](hmm.nbe))
    var psis = Swappable(new Array[StateSeq](hmm.nbe), new Array[StateSeq](hmm.nbe))

    val o1 = observables.next()
    val E1 = hmm.E(o1)
    for (j <- 0 to hmm.nbe - 1) {
      deltas.current(j) = hmm.PI(j) + E1(j)
      psis.current(j) = StateSeq().extend(j)
    }
    deltas = deltas.swap()
    psis = psis.swap()

    observables.foreach { o =>
      val E = hmm.E(o)

      for (j <- 0 to hmm.nbe - 1) {
        val (max, argMax) = maxArgMax(0 to hmm.nbe - 1,
          i => deltas.last(i) + hmm.T(i)(j) + E(j)
        )
        deltas.current(j) = max
        psis.current(j) = psis.last(argMax).extend(j)
      }
      deltas = deltas.swap()
      psis = psis.swap()
    }

    val (_, argMax) = maxArgMax(0 to hmm.nbe - 1, i => deltas.last(i))
    psis.last(argMax)
  }

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

  case class Swappable[T](current: Array[T], last: Array[T]) {
    def apply(i: Int): T = last(i)

    def update(i: Int, v: T) = current(i) = v

    def swap(): Swappable[T] = Swappable(last, current)
  }

  def trainHMM(nbe: Int, trainCorpus: Iterator[CorpusItem], devCorpus: Iterator[CorpusItem]): HiddenMarkovModel = {
    val categoryCounts: Array[Int] = Array.ofDim(nbe)
    val startingCategoryCounts: Array[Int] = Array.ofDim(nbe)
    val perCategoryCategoryCounts: Array[Array[Int]] = Array.ofDim(nbe, nbe)
    var perWordCategoryCounts: Map[Int, Array[Int]] = Map()
    var unknownWordCounts: Int = 0
    val unknownWordCategoryCounts: Array[Int] = Array.ofDim(nbe)

    var lastCat: Option[Int] = None
    trainCorpus.foreach {
      case WordCategoryPair(word, cat) =>
        categoryCounts(cat) += 1

        if (!perWordCategoryCounts.contains(word)) {
          perWordCategoryCounts += word -> Array.ofDim(nbe)
        }
        perWordCategoryCounts(word)(cat) += 1

        if (lastCat == None) {
          startingCategoryCounts(cat) += 1
          lastCat = Some(cat)
        } else {
          perCategoryCategoryCounts(lastCat.get)(cat) += 1
        }
      case SentenceSeparator =>
        lastCat = None
    }

    devCorpus.foreach {
      case WordCategoryPair(word, cat) =>
        if (!perWordCategoryCounts.contains(word)) {
          unknownWordCounts += 1
          unknownWordCategoryCounts(cat) += 1
        }
      case SentenceSeparator =>
    }

    val PI: Array[Double] = Array.ofDim(nbe)
    val T: Array[Array[Double]] = Array.ofDim(nbe, nbe)
    var E: Map[Int, Array[Double]] = Map()
    val UE: Array[Double] = Array.ofDim(nbe)

    for (i <- 0 to nbe - 1) {
      PI(i) = Math.log(startingCategoryCounts(i).toDouble / categoryCounts(i).toDouble)

      for (j <- 0 to nbe - 1) {
        T(i)(j) = Math.log(perCategoryCategoryCounts(i)(j).toDouble / categoryCounts(i).toDouble)
      }

      UE(i) = Math.log(unknownWordCategoryCounts(i).toDouble / unknownWordCounts.toDouble)
    }

    perWordCategoryCounts.foreach { case (o, wordCategoryCounts) =>
      val emitProbabilities: Array[Double] = Array.ofDim(nbe)
      for (i <- 0 to nbe - 1) {
        emitProbabilities(i) = Math.log(wordCategoryCounts(i).toDouble / categoryCounts(i).toDouble)
      }
      E += o -> emitProbabilities
    }

    HiddenMarkovModel(nbe, PI, T, E, UE)
  }

  trait StateSeq {
    def extend(state: Int) = StateSeqN(state, this)

    def toSeq: Seq[Int] = {
      def revSeq(seq: StateSeq, tail: List[Int]): List[Int] = seq match {
        case StateSeq0 => tail
        case StateSeqN(state, previous) => revSeq(previous, state :: tail)
      }
      revSeq(this, Nil)
    }
  }

  case class StateSeqN(state: Int, previous: StateSeq) extends StateSeq

  case object StateSeq0 extends StateSeq

  object StateSeq {
    def apply() = StateSeq0
  }

}

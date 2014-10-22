package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._

import scala.annotation.tailrec
import scala.reflect.ClassTag

import scala.collection._

object RelFreqSimpleTrainer extends Trainer {

  import Corpora._
  import Utils._

  def train(breadth: Int, depth: Int, corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
    val size = pow(breadth, depth)

    val allInitialCategoryCounts = MatrixTree[Int](breadth, depth)
    val perInitialCategoryCounts = MatrixTree[Int](breadth, depth)

    val allWordCategoryCounts = Array.ofDim[Int](breadth)
    val perWordCategoryCounts: mutable.Map[Int, Array[Int]] = mutable.Map()
    val perWordCounts: mutable.Map[Int, Int] = mutable.Map()
    val unknownWordCategoryCounts: Array[Int] = Array.ofDim(breadth)

    corpus.sequences.foreach { s: Sequence with Annotation =>
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
          perWordCounts += word -> 0
          perWordCategoryCounts += word -> Array.ofDim(breadth)
        }
        perWordCounts(word) += 1
        perWordCategoryCounts(word)(cat) += 1
        allWordCategoryCounts(cat) += 1
      }
    }

    val T = MatrixTree[Double](breadth, depth)
    var E: mutable.Map[Int, Array[Double]] = mutable.Map()
    val UE: Array[Double] = Array.ofDim(breadth)

    for (d <- 0 to depth) {
      for (i <- 0 until pow(breadth, d)) {
        for (j <- 0 until breadth) {
          var probability = log(perInitialCategoryCounts(d)(j)(i)) - log(allInitialCategoryCounts(d)(0)(i))

          // Drift hacks
          //if (probability.isNegInfinity) probability = 1.0E-307
          if (probability.isNaN) probability = Double.NegativeInfinity

          T(d)(j)(i) = probability
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

        if (perWordCounts(o) == 1) {
          var cat = -1
          for (j <- 0 until breadth) {
            if (wordCategoryCounts(j) == 1) cat = j
          }

          unknownWordCategoryCounts(cat) += 1
          allWordCategoryCounts(cat) += 1
        }
    }

    for (j <- 0 until breadth) {
      UE(j) = log(unknownWordCategoryCounts(j)) - log(allWordCategoryCounts(j))
    }

    HiddenMarkovModel(breadth, depth, T, E, UE)
  }
}

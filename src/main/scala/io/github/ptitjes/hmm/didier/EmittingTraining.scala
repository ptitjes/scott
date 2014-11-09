package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.IntParameter
import io.github.ptitjes.hmm.Utils._

import scala.collection.mutable

object EmittingTraining {

  object UNKNOWN_THRESHOLD extends IntParameter("Unknown Word Threshold", 18)

  def train(breadth: Int, corpus: Corpus[Sequence with Annotation], threshold: Int): (mutable.Map[Int, Array[Double]], Array[Double]) = {
    val allWordCategoryCounts = Array.ofDim[Int](breadth)
    val perWordCategoryCounts: mutable.Map[Int, Array[Int]] = mutable.Map()
    val perWordCounts: mutable.Map[Int, Int] = mutable.Map()
    val unknownWordCategoryCounts: Array[Int] = Array.ofDim(breadth)

    corpus.sequences.foreach { s: Sequence with Annotation =>
      s.observablesAndStates.foreach { case (word, cat) =>

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

    var E: mutable.Map[Int, Array[Double]] = mutable.Map()
    val UE: Array[Double] = Array.ofDim(breadth)

    if (threshold != 0) {
      perWordCategoryCounts.foreach {
        case (o, wordCategoryCounts) =>
          if (perWordCounts(o) <= threshold) {
            for (j <- 0 until breadth) {
              unknownWordCategoryCounts(j) += wordCategoryCounts(j)
              allWordCategoryCounts(j) += 1
            }
          }
      }
    }

    perWordCategoryCounts.foreach {
      case (o, wordCategoryCounts) =>
        val emitProbabilities: Array[Double] = Array.ofDim(breadth)
        for (j <- 0 until breadth) {
          emitProbabilities(j) = avoidInfinity(log(wordCategoryCounts(j)) - log(allWordCategoryCounts(j)))
        }
        E += o -> emitProbabilities
    }

    if (threshold != 0) {
      for (j <- 0 until breadth) {
        UE(j) = avoidInfinity(log(unknownWordCategoryCounts(j)) - log(allWordCategoryCounts(j)))
      }
    } else {
      for (j <- 0 until breadth) {
        UE(j) = avoidInfinity(-log(breadth))
      }
    }
    (E, UE)
  }
}

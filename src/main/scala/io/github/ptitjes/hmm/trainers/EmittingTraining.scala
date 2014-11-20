package io.github.ptitjes.hmm.trainers

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.{Word, IntParameter}
import io.github.ptitjes.hmm.Utils._

import scala.collection._
import scala.collection.mutable

object EmittingTraining {

	object UNKNOWN_THRESHOLD extends IntParameter("Unknown Word Threshold", 18)

	def train(breadth: Int, corpus: Corpus[Sequence with Annotation], threshold: Int)
	: (mutable.Map[Int, Array[Double]], Array[Double], Map[Int, BitSet]) = {

		val dictionary: mutable.Map[Word, Set[Int]] = mutable.Map()
		val allWordCategoryCounts = Array.ofDim[Int](breadth)
		val perWordCategoryCounts: mutable.Map[Int, Array[Int]] = mutable.Map()
		val perWordCounts: mutable.Map[Int, Int] = mutable.Map()
		val unknownWordCategoryCounts: Array[Int] = Array.ofDim(breadth)

		corpus.sequences.foreach { s: Sequence with Annotation =>
			s.observablesAndStates.foreach { case (word, tag) =>
				if (!dictionary.contains(word))
					dictionary(word) = Set(tag)
				else
					dictionary(word) += tag

				// Emission counts
				if (!perWordCategoryCounts.contains(word.code)) {
					perWordCounts += word.code -> 0
					perWordCategoryCounts += word.code -> Array.ofDim(breadth)
				}
				perWordCounts(word.code) += 1
				perWordCategoryCounts(word.code)(tag) += 1
				allWordCategoryCounts(tag) += 1
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
		(E, UE, dictionary.map { case (word, tags) => (word.code, BitSet() ++ tags)})
	}
}

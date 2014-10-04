package io.github.ptitjes.hmm

import java.io.File

import io.github.ptitjes.hmm.Corpora.{SentenceSeparator, WordCategoryPair}

import scala.collection.mutable.ArrayBuffer

object test extends App {

  import io.github.ptitjes.hmm.didier.Viterbi._

  val DEBUG = false
  val PATH = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/"

  val trainCorpus = timed("Open train corpus file") {
    Corpora.fromFile(new File(PATH + "ftb.train.encode"))
  }

  val devCorpus = timed("Open dev corpus file") {
    Corpora.fromFile(new File(PATH + "ftb.dev.encode"))
  }

  val hmm = timed("Train HMM") {
    trainHMM(15, 1, trainCorpus, devCorpus)
  }

  val testCorpus = timed("Open test corpus file") {
    Corpora.fromFile(new File(PATH + "ftb.test.encode"))
  }

  timed("Test HMM") {
    val sentence: ArrayBuffer[Int] = ArrayBuffer()
    val categories: ArrayBuffer[Int] = ArrayBuffer()

    var words = 0
    var errors = 0

    testCorpus.foreach {
      case WordCategoryPair(word, category) =>
        sentence += word
        categories += category
        words += 1

      case SentenceSeparator =>
        val hypCategories = mostProbableStateSequence(sentence.iterator, hmm)
        sentence.zip(categories.zip(hypCategories)).foreach {
          case (word, (ref, hyp)) =>
            if (ref != hyp) {
              errors += 1
              if (DEBUG) print(">")
            }
            if (DEBUG) println(s"$word $ref $hyp")
        }
        if (DEBUG) println()

        sentence.clear()
        categories.clear()
    }

    val errorRate = errors.toDouble / words.toDouble
    val accuracy = 1 - errorRate
    println(s"Errors: $errors; Words = $words; Accuracy = $accuracy.")
  }

  def timed[T](step: String)(execution: => T): T = {
    val start = System.currentTimeMillis()
    val result = execution
    val time = System.currentTimeMillis() - start
    println(step + ": " + time + "ms")
    result
  }
}

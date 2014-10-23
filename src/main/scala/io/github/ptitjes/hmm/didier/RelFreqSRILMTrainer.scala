package io.github.ptitjes.hmm.didier

import java.io._

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.Trainer._

import scala.io.Source
import scala.collection.{mutable => mutable}

object RelFreqSRILMTrainer extends Algorithm[Trainer] {

  val PATH_TO_SRILM = "/home/didier/Documents/Work/Master/DM/InfStat/srilm/bin/i686-m64"

  def name: String = "RelFreq-SRILM"

  override def parameters: Set[Parameter[_]] = Set(ORDER)

  def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

  class Instance(configuration: Configuration) extends Trainer {

    import io.github.ptitjes.hmm.Corpora._
    import io.github.ptitjes.hmm.Utils._

    val SENTENCE_START = -1
    val SENTENCE_STOP = -2

    def train(breadth: Int, corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
      val depth = configuration(ORDER)

      val ngrams = runNgramCount(depth + 1, corpus)

      def probabilityFor(ngram: Seq[Int]): Double = {
        if (ngrams.contains(ngram)) ngrams(ngram).probability
        else {
          val left = ngram.drop(1)
          val right = ngram.dropRight(1)
          probabilityFor(right) + (if (ngrams.contains(left)) ngrams(left).backoffWeight else 0.0)
        }
      }

      def ngramSeq(level: Int, previousState: Int, c: Int): Seq[Int] = {
        var d = 0
        var state = previousState
        var seq = new mutable.ArrayBuffer[Int]()

        seq += c
        for (l <- 0 until level) {
          seq += state % breadth
          state = state / breadth
          d += 1
        }

        if (level < depth) {
          seq += SENTENCE_START
        }

        seq.reverse.toList
      }

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

      val T = MatrixTree[Double](breadth, depth)
      var E: mutable.Map[Int, Array[Double]] = mutable.Map()
      val UE: Array[Double] = Array.ofDim(breadth)

      for (d <- 0 to depth) {
        for (i <- 0 until pow(breadth, d)) {
          for (j <- 0 until breadth) {
            val seq = ngramSeq(d, i, j)
            val probability = probabilityFor(seq)
            //println(seq + " " + probability)
            T(d)(j)(i) = probability
          }
        }
      }

      perWordCategoryCounts.foreach {
        case (o, wordCategoryCounts) =>
          if (perWordCounts(o) < 5) {
            for (j <- 0 until breadth) {
              unknownWordCategoryCounts(j) += wordCategoryCounts(j)
              allWordCategoryCounts(j) += 1
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

    def runNgramCount(order: Int, corpus: Corpus[Sequence with Annotation]): mutable.Map[Seq[Int], NGramData] = {
      val sentencesFilename = "temp.txt"

      using(new FileWriter(new File(sentencesFilename), true)) {
        fileWriter => using(new PrintWriter(fileWriter)) {
          out => corpus.sequences.foreach {
            s => out.println(s.observablesAndStates.map { case (o, c) => c}.mkString(" "))
          }
        }
      }

      val lmFilename = "temp.lm"
      val builder = new ProcessBuilder(PATH_TO_SRILM + "/ngram-count",
        "-order", order.toString,
        "-interpolate", "-wbdiscount",
        "-text", sentencesFilename,
        "-lm", lmFilename
      )
      val proc = builder.start()
      if (proc.waitFor() != 0) throw new IllegalStateException()

      parseARPA(order, lmFilename)
    }

    def parseARPA(order: Int, lmFilename: String): mutable.Map[Seq[Int], NGramData] = {
      val lines = Source.fromFile(new File(lmFilename)).getLines()

      lines.next()
      if (lines.next() != "\\data\\") throw new IllegalStateException()

      val ngramCounts = Array.ofDim[Int](order)
      for (i <- 1 to order) {
        ngramCounts(i - 1) = lines.next().split('=')(1).toInt
      }

      var ngrams = mutable.Map[Seq[Int], NGramData]()
      for (i <- 1 to order) {
        lines.next()
        if (lines.next() != "\\" + i + "-grams:") throw new IllegalStateException()

        for (j <- 0 until ngramCounts(i - 1)) {
          val split = lines.next().split('\t')
          val ngram = split(1).split(' ')

          ngrams += ngram.toList.map {
            case "<s>" => SENTENCE_START
            case "</s>" => SENTENCE_START
            case s => s.toInt
          } -> NGramData(split(0).toDouble,
            if (split.length == 3) split(2).toDouble else Double.NegativeInfinity)
        }
      }

      lines.next()
      if (lines.next() != "\\end\\") throw new IllegalStateException()
      ngrams
    }

    case class NGramData(probability: Double, backoffWeight: Double)

  }

}

package io.github.ptitjes.hmm.trainers

import java.io._

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm._

import scala.collection.mutable
import scala.io.Source

object RelFreqDiscountingTrainer extends Algorithm[Trainer] {

	val PATH_TO_SRILM = "/home/didier/Documents/Work/Master/DM/InfStat/srilm/bin/i686-m64"

	val WORK_DIRECTORY = "temp"
	val TEMP_FILENAME_PREFIX = "srilm-"
	val SENTENCES_FILENAME_SUFFIX = "-sentences.txt"
	val LM_FILENAME_SUFFIX = "-interpolated.lm"

	def name: String = "Freq-WB"

	override def parameters: Set[Parameter[_]] = Set(
		ORDER, EmittingTraining.UNKNOWN_THRESHOLD, MULTIPLIER
	)

	object MULTIPLIER extends IntParameter("Multiplier", 8)

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer {

		import io.github.ptitjes.hmm.Corpora._
		import io.github.ptitjes.hmm.Utils._

		val SENTENCE_START = -1
		val SENTENCE_STOP = -2

		def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
			val breadth = stateCount(corpus)
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

			val T = initializeMatrixTree[Double](breadth, depth)

			for (d <- 0 to depth) {
				for (i <- 0 until pow(breadth, d)) {
					for (j <- 0 until breadth) {
						val seq = ngramSeq(d, i, j)
						val probability = probabilityFor(seq)
						T(d)(j)(i) = probability
					}
				}
			}

			val (e, ue) = EmittingTraining.train(breadth, corpus, configuration(EmittingTraining.UNKNOWN_THRESHOLD))

			HMMGenerative(breadth, depth, T, e, ue)
		}

		def runNgramCount(order: Int, corpus: Corpus[Sequence with Annotation]): mutable.Map[Seq[Int], NGramData] = {

			val workDirectory = new File(WORK_DIRECTORY)
			if (!workDirectory.exists()) workDirectory.mkdirs()

			val sentencesFile = File.createTempFile(TEMP_FILENAME_PREFIX, SENTENCES_FILENAME_SUFFIX, workDirectory)
			val lmFile = File.createTempFile(TEMP_FILENAME_PREFIX, LM_FILENAME_SUFFIX, workDirectory)

			sentencesFile.deleteOnExit()
			lmFile.deleteOnExit()

			using(new FileWriter(sentencesFile)) {
				fileWriter => using(new PrintWriter(fileWriter)) {
					out =>
						for (i <- 1 to configuration(MULTIPLIER)) {
							corpus.sequences.foreach {
								s => out.println(s.observablesAndStates.map { case (o, c) => c}.mkString(" "))
							}
						}
				}
			}

			val builder = new ProcessBuilder(PATH_TO_SRILM + "/ngram-count",
				"-order", order.toString,
				"-interpolate", "-wbdiscount",
				"-text", sentencesFile.getName,
				"-lm", lmFile.getName
			)
			builder.directory(workDirectory)
			builder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
			builder.redirectError(ProcessBuilder.Redirect.INHERIT)
			val proc = builder.start()
			if (proc.waitFor() != 0) throw new IllegalStateException()

			parseARPA(order, lmFile)
		}

		def parseARPA(order: Int, lmFile: File): mutable.Map[Seq[Int], NGramData] = {
			val lines = Source.fromFile(lmFile).getLines()

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

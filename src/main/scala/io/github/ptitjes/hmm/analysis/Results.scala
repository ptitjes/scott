package io.github.ptitjes.hmm.analysis

import java.io.PrintWriter

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.decoders.BeamDecoder

case class Results(globalCounts: ErrorCount,
                   perCategoryCounts: Array[ErrorCount],
                   trainingElapsedTime: Long, decodingElapsedTime: Long) {

	def errorRate = globalCounts.errorRate

	def accuracy = globalCounts.accuracy

	def unknownErrorRate = globalCounts.unknownErrorRate

	def unknownAccuracy = globalCounts.unknownAccuracy

	def unknownErrorRatio = globalCounts.unknownErrorRatio

	def categoryErrorRatio(i: Int) = perCategoryCounts(i).errors.toDouble / globalCounts.errors

	def display(): Unit = {
		println(this)
		for (i <- 0 until perCategoryCounts.length)
			println(f"\tCategory: ${
				Lexica.CATEGORIES.padded(i)
			} ${
				perCategoryCounts(i)
			}; Of Total; ${
				categoryErrorRatio(i) * 100
			}%6.2f%%")
		println()
	}

	def printTo(out: PrintWriter): Unit = {
		out.println(this)
		for (i <- 0 until perCategoryCounts.length)
			out.println(f"\tCategory: ${
				Lexica.CATEGORIES.padded(i)
			} ${
				perCategoryCounts(i)
			}; Of Total; ${
				categoryErrorRatio(i) * 100
			}%6.2f%%")
		out.println()
	}

	override def toString: String = globalCounts +
		s"; TrainingTime: ${
			prettyTimeMs(trainingElapsedTime)
		} ms; DecodingTime: ${
			prettyTimeMs(decodingElapsedTime)
		} ms."
}

class ErrorCount {
	var errors: Int = 0
	var total: Int = 0
	var unknownErrors: Int = 0
	var unknownTotal: Int = 0

	def errorRate = errors.toDouble / total.toDouble

	def accuracy = 1 - errorRate

	def unknownErrorRate = unknownErrors.toDouble / unknownTotal.toDouble

	def unknownAccuracy = 1 - unknownErrorRate

	def unknownErrorRatio = unknownErrors.toDouble / errors.toDouble

	override def toString: String =
		f"Errors: ${
			errors
		}%5d; Total: ${
			total
		}%5d; UnknownErrors: ${
			unknownErrors
		}%5d; UnknownTotal: ${
			unknownTotal
		}%5d Accuracy: ${
			accuracy * 100
		}%6.2f%%; UnknownAccuracy: ${
			unknownAccuracy * 100
		}%6.2f%%; UnknownErrorRatio: ${
			unknownErrorRatio * 100
		}%6.2f%%"
}

object Results {

	def trainDecodeAndCheck(conf: Configuration,
	                        trainCorpus: Corpus[Sequence with Annotation],
	                        testCorpus: Corpus[Sequence with Annotation],
	                        debug: Boolean): Results = {

		val trainer = conf(Analysis.TRAINER).instantiate(conf)
		val decoder = conf(Analysis.DECODER).instantiate(conf)
		trainDecodeAndCheck(trainer, decoder, trainCorpus, testCorpus, debug)
	}

	def trainDecodeAndCheck(trainer: Trainer,
	                        decoder: Decoder,
	                        trainCorpus: Corpus[Sequence with Annotation],
	                        testCorpus: Corpus[Sequence with Annotation],
	                        debug: Boolean = false): Results = {

		val (hmm, trainingElapsedTime) = timed {
			trainer.train(trainCorpus)
		}

		val (hypCorpus, decodingElapsedTime) = timed {
			decoder.setHmm(hmm)
			decoder.decode(testCorpus)
		}

		check(hmm, testCorpus, hypCorpus, trainingElapsedTime, decodingElapsedTime, debug)
	}

	def decodeAndCheck(hmm: HiddenMarkovModel,
	                   decoder: Decoder,
	                   testCorpus: Corpus[Sequence with Annotation],
	                   debug: Boolean = false): Results = {

		val (hypCorpus, decodingElapsedTime) = timed {
			decoder.setHmm(hmm)
			decoder.decode(testCorpus)
		}

		check(hmm, testCorpus, hypCorpus, 0, decodingElapsedTime, debug)
	}

	def check(hmm: HiddenMarkovModel,
	          refCorpus: Corpus[Sequence with Annotation],
	          hypCorpus: Corpus[Sequence with Annotation],
	          trainingElapsedTime: Long, decodingElapsedTime: Long,
	          debug: Boolean = false): Results = {

		check(hmm, refCorpus, hypCorpus,
			trainingElapsedTime, decodingElapsedTime, debug, new PrintWriter(System.out))
	}

	def check(hmm: HiddenMarkovModel,
	          refCorpus: Corpus[Sequence with Annotation],
	          hypCorpus: Corpus[Sequence with Annotation],
	          trainingElapsedTime: Long, decodingElapsedTime: Long,
	          debug: Boolean, out: PrintWriter): Results = {

		val globalCounts = new ErrorCount
		val perCategoryCounts = Array.fill(stateCount(refCorpus))(new ErrorCount)

		refCorpus.sequences.zip(hypCorpus.sequences).foreach {
			case (refSeq, hypSeq) =>

				if (refSeq.observablesAndStates.length != hypSeq.observablesAndStates.length) {
					throw new IllegalStateException("Observable length mismatch!")
				}

				refSeq.observablesAndStates.zip(hypSeq.observablesAndStates).foreach {
					case ((oRef, sRef), (oHyp, sHyp)) =>
						if (oRef != oHyp) {
							throw new IllegalStateException("Observable mismatch!")
						}
						val error = sRef != sHyp

						globalCounts.total += 1
						perCategoryCounts(sRef).total += 1
						if (error) {
							globalCounts.errors += 1
							perCategoryCounts(sRef).errors += 1
						}

						if (hmm.isUnknown(oRef)) {
							globalCounts.unknownTotal += 1
							perCategoryCounts(sRef).unknownTotal += 1
							if (error) {
								globalCounts.unknownErrors += 1
								perCategoryCounts(sRef).unknownErrors += 1
							}
						}

						if (debug) {
							out.print(if (error) ">" else " ")
							out.print(f"${oRef.code}%6d")
							out.print(if (hmm.isUnknown(oRef)) " U" else "  ")
							out.print(f"\t$sRef%2d ${Lexica.CATEGORIES(sRef)}%-5s")
							out.print(f"\t$sHyp%2d ${Lexica.CATEGORIES(sHyp)}%-5s\t")
							out.println(oRef.string)
						}
				}
				if (debug) out.println()
		}

		val results = Results(globalCounts, perCategoryCounts, trainingElapsedTime, decodingElapsedTime)
		if (debug) results.printTo(out)

		results
	}
}

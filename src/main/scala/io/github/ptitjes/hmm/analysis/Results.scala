package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.{HiddenMarkovModel, Lexica, Trainer, Decoder}

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

	override def toString: String = globalCounts +
		s"; TrainingTime: ${
			trainingElapsedTime
		} ms; DecodingTime: ${
			decodingElapsedTime
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

		check(trainCorpus, testCorpus, hmm, hypCorpus,
			trainingElapsedTime, decodingElapsedTime, debug)
	}

	def decodeAndCheck(hmm: HiddenMarkovModel,
	                   decoder: Decoder,
	                   trainCorpus: Corpus[Sequence with Annotation],
	                   testCorpus: Corpus[Sequence with Annotation],
	                   debug: Boolean = false): Results = {

		val (hypCorpus, decodingElapsedTime) = timed {
			decoder.setHmm(hmm)
			decoder.decode(testCorpus)
		}

		check(trainCorpus, testCorpus, hmm, hypCorpus,
			0, decodingElapsedTime, debug)
	}

	def check(trainCorpus: Corpus[Sequence with Annotation],
	          testCorpus: Corpus[Sequence with Annotation],
	          hmm: HiddenMarkovModel,
	          hypCorpus: Corpus[Sequence with Annotation],
	          trainingElapsedTime: Long, decodingElapsedTime: Long,
	          debug: Boolean = false): Results = {

		val globalCounts = new ErrorCount
		val perCategoryCounts = Array.fill(stateCount(trainCorpus))(new ErrorCount)

		testCorpus.sequences.zip(hypCorpus.sequences).foreach {
			case (refSeq, hypSeq) =>

				if (refSeq.observables.length != hypSeq.observables.length || refSeq.states.length != hypSeq.states.length) {
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
							print(if (error) ">" else " ")
							print(f"$oRef%6d")
							print(if (hmm.isUnknown(oRef)) " U" else "  ")
							print(f"\t$sRef%2d ${Lexica.CATEGORIES(sRef)}%-5s")
							print(f"\t$sHyp%2d ${Lexica.CATEGORIES(sHyp)}%-5s\t")
							println(Lexica.WORDS(oRef))
						}
				}
				if (debug) println()
		}

		Results(globalCounts, perCategoryCounts, trainingElapsedTime, decodingElapsedTime)
	}
}

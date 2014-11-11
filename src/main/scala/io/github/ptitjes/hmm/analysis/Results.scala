package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.{HiddenMarkovModel, Lexica, Trainer, Decoder}

case class Results(globalCounts: ErrorCount,
                   perCategoryCounts: Array[ErrorCount],
                   trainingElapsedTime: Long, decodingElapsedTime: Long) {

  def errorRate = globalCounts.errors.toDouble / globalCounts.total.toDouble

  def accuracy = 1 - errorRate

  def unknownErrorRate = globalCounts.unknownErrors.toDouble / globalCounts.unknownTotal.toDouble

  def unknownAccuracy = 1 - unknownErrorRate

  def unknownErrorRatio = globalCounts.unknownErrors.toDouble / globalCounts.errors.toDouble

  def display(): Unit = {
    println(this)
    for (i <- 0 until perCategoryCounts.length)
      println(s"\tCategory: ${Lexica.CATEGORIES.padded(i)} > ${perCategoryCounts(i)}")
    println()
  }

  override def toString: String = f"Errors: ${
    globalCounts.errors
  }%d; Words: ${
    globalCounts.total
  }%d; Accuracy: ${
    accuracy * 100
  }%2.2f%%; UnknownAccuracy: ${
    unknownAccuracy * 100
  }%2.2f%%; UnknownErrorRatio: ${
    unknownErrorRatio * 100
  }%2.2f%%; TrainingTime: ${
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

  override def toString: String = f"Errors: ${
    errors
  }%5d; Total: ${
    total
  }%5d; UnknownErrors: ${
    unknownErrors
  }%5d; UnknownTotal: ${
    unknownTotal
  }%5d"
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
		  trainingElapsedTime, decodingElapsedTime , debug)
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
							print(if (hmm.isUnknown(oRef)) "     U" else f"$oRef%6d")
							print(f"\t$sRef%2d\t$sHyp%2d\t")
							println(Lexica.WORDS(oRef))
						}
				}
				if (debug) println()
		}

		Results(globalCounts, perCategoryCounts, trainingElapsedTime, decodingElapsedTime)
	}
}

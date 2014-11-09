package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm.Corpora.{Annotation, Corpus, Sequence}
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.{Trainer, Decoder}

case class Results(errors: Int, wordCount: Int,
                   unknownErrors: Int, unknownWordCount: Int,
                   trainingElapsedTime: Long, decodingElapsedTime: Long) {

  def errorRate = errors.toDouble / wordCount.toDouble

  def accuracy = 1 - errorRate

  def unknownErrorRate = unknownErrors.toDouble / unknownWordCount.toDouble

  def unknownAccuracy = 1 - unknownErrorRate

  def unknownErrorRatio = unknownErrors.toDouble / errors.toDouble

  override def toString: String = f"Errors: ${
    errors
  }%d; Words = ${
    wordCount
  }%d; Accuracy = ${
    accuracy * 100
  }%2.2f%%; UnknownAccuracy: ${
    unknownAccuracy * 100
  }%2.2f%%; UnknownErrorRatio: ${
    unknownErrorRatio * 100
  }%2.2f%%; TrainingTime = ${
    trainingElapsedTime
  } ms; DecodingTime = ${
    decodingElapsedTime
  } ms."
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

    var errors = 0
    var unknownErrors = 0
    var unknownWordCount = 0
    var words = 0

    val (hypCorpus, decodingElapsedTime) = timed {
      decoder.setHmm(hmm)
      decoder.decode(testCorpus)
    }

    testCorpus.sequences.zip(hypCorpus.sequences).foreach {
      case (refSeq, hypSeq) =>

        if (refSeq.observables.length != hypSeq.observables.length || refSeq.states.length != hypSeq.states.length) {
          throw new IllegalStateException("Observable length mismatch!")
        }

        words += refSeq.observables.length
        refSeq.observablesAndStates.zip(hypSeq.observablesAndStates).foreach {
          case ((oRef, sRef), (oHyp, sHyp)) =>
            if (oRef != oHyp) {
              throw new IllegalStateException("Observable mismatch!")
            }
            val error = sRef != sHyp
            if (error) {
              errors += 1
            }
            if (hmm.isUnknown(oRef)) {
              unknownWordCount += 1
              if (error) unknownErrors += 1
            }

            if (debug) {
              print(if (error) ">" else " ")
              print(if (hmm.isUnknown(oRef)) "     U" else f"$oRef%6d")
              println(f"\t$sRef%2d\t$sHyp%2d")
            }
        }
        if (debug) println()
    }

    Results(errors, words, unknownErrors, unknownWordCount, trainingElapsedTime, decodingElapsedTime)
  }
}

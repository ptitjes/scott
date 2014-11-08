package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm.Corpora.{Annotation, Sequence, Corpus}
import io.github.ptitjes.hmm.{Decoder, HiddenMarkovModel}
import io.github.ptitjes.hmm.Utils._

case class Results(errors: Int, words: Int,
                   unknownErrorRate: Double,
                   accuracy: Double, unknownAccuracy: Double,
                   ellapsedTime: Long) {

  override def toString: String = f"Errors: ${
    errors
  }%d; Words = ${
    words
  }%d; UnknownErrorRate: ${
    unknownErrorRate * 100
  }%2.2f%%; Accuracy = ${
    accuracy * 100
  }%2.2f%%; UnknownAccuracy: ${
    unknownAccuracy * 100
  }%2.2f%%; EllapsedTime = ${
    ellapsedTime
  } ms."
}

object Results {

  def decodeAndCheck(decoder: Decoder, hmm: HiddenMarkovModel,
                     refCorpus: Corpus[Sequence with Annotation], debug: Boolean = false): Results = {
    var errors = 0
    var errorsOnUnknowns = 0
    var accurateUnknowns = 0
    var unknownCount = 0
    var words = 0

    val (hypCorpus, ellapsedTime) = timed {
      decoder.decode(hmm, refCorpus)
    }

    refCorpus.sequences.zip(hypCorpus.sequences).foreach {
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
              unknownCount += 1
              if (error) errorsOnUnknowns += 1
              else accurateUnknowns += 1
            }

            if (debug) {
              print(if (error) ">" else " ")
              print(if (hmm.isUnknown(oRef)) "     U" else f"$oRef%6d")
              println(f"\t$sRef%2d\t$sHyp%2d")
            }
        }
        if (debug) println()
    }

    val errorRate = errors.toDouble / words.toDouble
    val accuracy = 1 - errorRate

    Results(errors, words, errorsOnUnknowns.toDouble / errors, accuracy, accurateUnknowns.toDouble / unknownCount, ellapsedTime)
  }
}

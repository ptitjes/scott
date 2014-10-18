package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._

trait Algorithms {

  def trainWithRelativeFrequence(breadth: Int, depth: Int,
                                 train: Corpus[Sequence with Annotation],
                                 dev: Corpus[Sequence with Annotation]): HiddenMarkovModel

  def trainWithPerceptron(breadth: Int, depth: Int,
                          train: Corpus[Sequence with Annotation],
                          dev: Corpus[Sequence with Annotation]): HiddenMarkovModel

  def mostProbableSequences(hmm: HiddenMarkovModel,
                            test: Corpus[Sequence]): Corpus[Sequence with Annotation]
}

object Algorithms {

  val DEBUG = false

  case class Results(errors: Int, words: Int, unknownErrorRate: Double, accuracy: Double, unknownAccuracy: Double, ellapsedTime: Long) {
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

  def mostProbableStateSequences(algo: Algorithms, hmm: HiddenMarkovModel, refCorpus: Corpus[Sequence with Annotation]): Results = {
    var errors = 0
    var errorsOnUnknowns = 0
    var accurateUnknowns = 0
    var unknownCount = 0
    var words = 0

    val (hypCorpus, ellapsedTime) = timed {
      algo.mostProbableSequences(hmm, refCorpus)
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

            if (DEBUG) {
              print(if (error) ">" else " ")
              print(if (hmm.isUnknown(oRef)) "     U" else f"$oRef%6d")
              println(f"\t$sRef%2d\t$sHyp%2d")
            }
        }
        if (DEBUG) println()
    }

    val errorRate = errors.toDouble / words.toDouble
    val accuracy = 1 - errorRate

    Results(errors, words, errorsOnUnknowns.toDouble / errors, accuracy, accurateUnknowns.toDouble / unknownCount, ellapsedTime)
  }

  def timed[T](execution: => T): (T, Long) = {
    val start = System.currentTimeMillis()
    val result = execution
    val time = System.currentTimeMillis() - start
    (result, time)
  }
}

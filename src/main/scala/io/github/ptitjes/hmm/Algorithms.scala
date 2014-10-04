package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._

trait Algorithms {

  def trainWithRelativeFrequence(breadth: Int, depth: Int,
                                 train: Corpus[Sequence with Annotation],
                                 dev: Corpus[Sequence with Annotation]): HiddenMarkovModel

  def trainWithPerceptron(breadth: Int, depth: Int,
                          train: Corpus[Sequence with Annotation],
                          dev: Corpus[Sequence with Annotation]): HiddenMarkovModel

  def mostProbableStateSequence(hmm: HiddenMarkovModel,
                                sequence: Sequence): Sequence with Annotation
}

object Algorithms {

  val DEBUG = false

  case class Results(errors: Int, words: Int, accuracy: Double)

  def mostProbableStateSequences(algo: Algorithms, hmm: HiddenMarkovModel, test: Corpus[Sequence with Annotation]): Results = {
    var errors = 0
    var words = 0

    test.sequences.foreach {
      ref =>

        val hyp = algo.mostProbableStateSequence(hmm, ref)
        if (ref.observables.length != hyp.observables.length || ref.states.length != hyp.states.length) {
          throw new IllegalStateException("Observable length mismatch!")
        }

        words += ref.observables.length
        ref.observablesAndStates.zip(hyp.observablesAndStates).foreach {
          case ((oRef, sRef), (oHyp, sHyp)) =>
            if (oRef != oHyp) {
              throw new IllegalStateException("Observable mismatch!")
            }
            if (sRef != sHyp) {
              errors += 1
              if (DEBUG) println(s"$oRef\t$ref\t$hyp\t<<<")
            } else {
              if (DEBUG) println(s"$oRef\t$ref\t$hyp")
            }
        }
        if (DEBUG) println()
    }

    val errorRate = errors.toDouble / words.toDouble
    val accuracy = 1 - errorRate
    if (DEBUG)
      println(s"Errors: $errors; Words = $words; Accuracy = $accuracy.")

    Results(errors, words, accuracy)
  }
}

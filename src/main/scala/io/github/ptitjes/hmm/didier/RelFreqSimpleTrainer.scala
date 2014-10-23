package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.Trainer._

import scala.collection.{mutable => mutable}

object RelFreqSimpleTrainer extends Algorithm[Trainer] {

  def name: String = "RelFreq"

  override def parameters: Set[Parameter[_]] = Set(ORDER)

  def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

  class Instance(configuration: Configuration) extends Trainer {

    import io.github.ptitjes.hmm.Corpora._
    import io.github.ptitjes.hmm.Utils._

    def train(breadth: Int, corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
      val depth = configuration(ORDER)

      val size = pow(breadth, depth)

      val allCategoryCounts = MatrixTree[Int](breadth, depth)
      val perCategoryCounts = MatrixTree[Int](breadth, depth)

      corpus.sequences.foreach { s: Sequence with Annotation =>
        var d = 0
        var previousState = 0

        s.observablesAndStates.foreach { case (word, cat) =>

          perCategoryCounts(d)(cat)(previousState) += 1
          allCategoryCounts(d)(0)(previousState) += 1

          if (d < depth) {
            d += 1
          }
          previousState = previousState * breadth + cat
          previousState = previousState % size
        }
      }

      val T = MatrixTree[Double](breadth, depth)

      for (d <- 0 to depth) {
        for (i <- 0 until pow(breadth, d)) {
          for (j <- 0 until breadth) {
            var probability = log(perCategoryCounts(d)(j)(i)) - log(allCategoryCounts(d)(0)(i))

            // Drift hacks
            if (probability.isNaN) probability = Double.NegativeInfinity

            T(d)(j)(i) = probability
          }
        }
      }

      val (e, ue) = EmittingTraining.train(breadth, corpus, configuration(EmittingTraining.UNKNOWN_THRESHOLD))

      HiddenMarkovModel(breadth, depth, T, e, ue)
    }
  }

}

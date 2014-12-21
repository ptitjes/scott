package io.github.ptitjes.scott.trainers

import io.github.ptitjes.scott.HiddenMarkovModel._
import io.github.ptitjes.scott.Trainer._
import io.github.ptitjes.scott._

object RelFreqTrainer extends Trainer.Factory {

	def name: String = "Freq"

	override def parameters: Set[Parameter[_]] = Set(
		ORDER, EmittingTraining.UNKNOWN_THRESHOLD
	)

	override def isIterative: Boolean = false

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer {

		import io.github.ptitjes.scott.Corpora._
		import io.github.ptitjes.scott.Utils._

		def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
			val breadth = stateCount(corpus)
			val depth = configuration(ORDER)

			val size = pow(breadth, depth)

			val allCategoryCounts = initializeMatrixTree[Int](breadth, depth)
			val perCategoryCounts = initializeMatrixTree[Int](breadth, depth)

			corpus.sequences.foreach { s: Sequence with Annotation =>
				var d = 0
				var previousState = 0

				s.observablesAndStates.foreach { case (word, tag) =>

					perCategoryCounts(d)(tag)(previousState) += 1
					allCategoryCounts(d)(0)(previousState) += 1

					if (d < depth) {
						d += 1
					}
					previousState = (previousState * breadth + tag) % size
				}
			}

			val T = initializeMatrixTree[Double](breadth, depth)

			for (d <- 0 to depth) {
				for (i <- 0 until pow(breadth, d)) {
					for (j <- 0 until breadth) {
						T(d)(j)(i) = avoidInfinity(log(perCategoryCounts(d)(j)(i)) - log(allCategoryCounts(d)(0)(i)))
					}
				}
			}

			val (e, ue, dict) = EmittingTraining.train(breadth, corpus,
				configuration(EmittingTraining.UNKNOWN_THRESHOLD))

			HMMGenerative(breadth, depth, T, e, ue, dict)
		}
	}

}

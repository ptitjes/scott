package io.github.ptitjes.hmm.trainers

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm._

object RelFreqTrainer extends Trainer.Factory {

	def name: String = "Freq"

	override def parameters: Set[Parameter[_]] = Set(
		ORDER, EmittingTraining.UNKNOWN_THRESHOLD
	)

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer {

		import io.github.ptitjes.hmm.Corpora._
		import io.github.ptitjes.hmm.Utils._

		def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
			val breadth = stateCount(corpus)
			val depth = configuration(ORDER)

			val size = pow(breadth, depth)

			val allCategoryCounts = initializeMatrixTree[Int](breadth, depth)
			val perCategoryCounts = initializeMatrixTree[Int](breadth, depth)

			corpus.sequences.foreach { s: Sequence with Annotation =>
				var d = 0
				var previousState = 0

				s.observablesAndStates.foreach { case (word, cat) =>

					perCategoryCounts(d)(cat)(previousState) += 1
					allCategoryCounts(d)(0)(previousState) += 1

					if (d < depth) {
						d += 1
					}
					previousState = (previousState * breadth + cat) % size
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

			val (e, ue) = EmittingTraining.train(breadth, corpus, configuration(EmittingTraining.UNKNOWN_THRESHOLD))

			HMMGenerative(breadth, depth, T, e, ue)
		}
	}

}

package io.github.ptitjes.scott.api

/**
 * @author Didier Villevalois
 */
trait IterativeTrainer[X, Y <: X] extends Trainer[X, Y] {

	def train(corpus: DataSet[Y]): HiddenMarkovModel[X, Y] = {
		var resultHmm: HiddenMarkovModel[X, Y] = null
		train(corpus, new IterationCallback[X, Y] {
			override def iterationDone(iteration: Int, hmm: HiddenMarkovModel[X, Y], elapsedTime: Long): Unit = {
				resultHmm = hmm
			}
		})
		resultHmm
	}

	def train(corpus: DataSet[Y], callback: IterationCallback[X, Y]): Unit
}

package io.github.ptitjes.scott

import io.github.ptitjes.scott.corpora._

trait Trainer[X, Y <: X] {

	def train(corpus: Corpus[Y]): HiddenMarkovModel[X, Y]
}

trait IterativeTrainer[X, Y <: X] extends Trainer[X, Y] {

	def train(corpus: Corpus[Y]): HiddenMarkovModel[X, Y] = {
		var resultHmm: HiddenMarkovModel[X, Y] = null
		train(corpus, new IterationCallback[X, Y] {
			override def iterationDone(iteration: Int, hmm: HiddenMarkovModel[X, Y], elapsedTime: Long): Unit = {
				resultHmm = hmm
			}
		})
		resultHmm
	}

	def train(corpus: Corpus[Y], callback: IterationCallback[X, Y]): Unit
}

trait IterationCallback[X, Y <: X] {

	def iterationDone(iteration: Int, hmm: HiddenMarkovModel[X, Y], elapsedTime: Long): Unit
}

package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._

import scala.collection._

trait Trainer {

	def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel
}

trait IterativeTrainer extends Trainer {

	def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
		var resultHmm: HiddenMarkovModel = null
		train(corpus, new IterationCallback {
			override def iterationDone(configuration: Configuration, hmm: HiddenMarkovModel, elapsedTime: Long): Unit = {
				resultHmm = hmm
			}
		})
		resultHmm
	}

	def train(corpus: Corpus[Sequence with Annotation], callback: IterationCallback): Unit
}

trait IterationCallback {

	def iterationDone(configuration: Configuration, hmm: HiddenMarkovModel, elapsedTime: Long): Unit
}

object Trainer {

	object ORDER extends IntParameter("Order", 3)

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	trait Factory {

		def name: String

		def parameters: Set[Parameter[_]] = Set()

		def instantiate(configuration: Configuration): Trainer
	}

}

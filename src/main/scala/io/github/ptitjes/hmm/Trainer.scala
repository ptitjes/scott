package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._

trait Trainer {

	def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel
}

object Trainer {

	object ORDER extends IntParameter("Order", 3)

	trait Factory {

		def name: String

		def parameters: Set[Parameter[_]] = Set()

		def instantiate(configuration: Configuration): Trainer
	}

}

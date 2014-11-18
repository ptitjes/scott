package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils.ProgressBar

trait Decoder {

	def decode(corpus: Corpus[Sequence]): Corpus[Sequence with Annotation] = {
		val progress = new ProgressBar(f"Decoding", corpus.sequences.length)
		progress.set(0)

		corpus.map { sequence =>
			val result = decode(sequence)
			progress.increment()
			result
		}
	}

	def decode(sequence: Sequence): Sequence with Annotation

}

object Decoder {

	trait Factory {

		def name: String

		def parameters: Set[Parameter[_]] = Set()

		def instantiate(hmm: HiddenMarkovModel, configuration: Configuration): Decoder
	}

}

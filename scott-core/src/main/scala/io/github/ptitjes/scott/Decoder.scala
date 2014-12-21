package io.github.ptitjes.scott

import io.github.ptitjes.scott.Corpora._
import io.github.ptitjes.scott.Utils.ProgressBar

import scala.collection._

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

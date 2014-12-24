package io.github.ptitjes.scott

import io.github.ptitjes.scott.Utils.ProgressBar
import io.github.ptitjes.scott.corpora.Annotation._
import io.github.ptitjes.scott.corpora._

import scala.collection._

trait Decoder {

	def decode(corpus: Corpus): Corpus = {
		val progress = new ProgressBar(f"Decoding", corpus.size)
		progress.set(0)

		corpus.map { sequence =>
			val result = decode(sequence)
			progress.increment()
			result
		}
	}

	def decode(sequence: Sentence): Sentence

}

object Decoder {

	trait Factory {

		def name: String

		def parameters: Set[Parameter[_]] = Set()

		def instantiate(hmm: HiddenMarkovModel, configuration: Configuration): Decoder
	}

}

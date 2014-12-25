package io.github.ptitjes.scott

import io.github.ptitjes.scott.Utils.ProgressBar
import io.github.ptitjes.scott.corpora._

trait Decoder[X, Y] {

	def decode(corpus: Corpus[X]): Corpus[Y] = {
		val progress = new ProgressBar(f"Decoding", corpus.size)
		progress.set(0)

		corpus.map { sequence =>
			val result = decode(sequence)
			progress.increment()
			result
		}
	}

	def decode(sequence: Sentence[X]): Sentence[Y]

}

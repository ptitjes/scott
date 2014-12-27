package io.github.ptitjes.scott.api

import io.github.ptitjes.scott.utils.Utils.ProgressBar

trait Decoder[X, Y] {

	def decode(corpus: DataSet[X]): DataSet[Y] = {
		val progress = new ProgressBar(f"Decoding", corpus.size)
		progress.set(0)

		corpus.map { sequence =>
			val result = decode(sequence)
			progress.increment()
			result
		}
	}

	def decode(sequence: Sequence[X]): Sequence[Y]

}

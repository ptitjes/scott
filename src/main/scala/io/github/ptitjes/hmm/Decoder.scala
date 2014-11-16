package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils.ProgressBar

trait Decoder {

	def setHmm(hmm: HiddenMarkovModel): Unit

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

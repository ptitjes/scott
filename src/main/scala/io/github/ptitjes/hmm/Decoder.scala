package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._

trait Decoder {

  def setHmm(hmm: HiddenMarkovModel): Unit

  def decode(corpus: Corpus[Sequence]): Corpus[Sequence with Annotation] = {
    corpus.map(sequence => decode(sequence))
  }

  def decode(sequence: Sequence): Sequence with Annotation
}

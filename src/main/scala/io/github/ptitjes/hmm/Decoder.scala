package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._

trait Decoder {

  def decode(hmm: HiddenMarkovModel, corpus: Corpus[Sequence]): Corpus[Sequence with Annotation]
}

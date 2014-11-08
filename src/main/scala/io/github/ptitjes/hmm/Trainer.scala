package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.Corpora._

trait Trainer {

  def train(breadth: Int, corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel
}

object Trainer {
  object ORDER extends IntParameter("Order", 3)
}

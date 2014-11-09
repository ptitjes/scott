package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Results._

object testRelFreq extends App {

  val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

  val conf = Configuration()
    .set(Trainer.ORDER, 1)
    .set(EmittingTraining.UNKNOWN_THRESHOLD, 0)

  val trainer = RelFreqTrainer.instantiate(conf)
  val decoder = FullDecoder.instantiate(conf)

  trainDecodeAndCheck(trainer, decoder, trainCorpus, trainCorpus).display()
  trainDecodeAndCheck(trainer, decoder, trainCorpus, devCorpus, debug = true).display()
}

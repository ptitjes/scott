package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

object testRelFreq extends App {

  val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

  val conf = Configuration()
    .set(Trainer.ORDER, 1)
    .set(EmittingTraining.UNKNOWN_THRESHOLD, 0)

  val trainer = RelFreqTrainer.instantiate(conf)
  val decoder = FullDecoder.instantiate(conf)

  val hmm = trainer.train(trainCorpus)

  import io.github.ptitjes.hmm.analysis.Results._

  timed("Test HMM") {
    println(decodeAndCheck(decoder, hmm, trainCorpus))
    println(decodeAndCheck(decoder, hmm, devCorpus))
  }
}

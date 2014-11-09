package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

object testDiscounting extends App {

  val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

  val conf = Configuration().set(Trainer.ORDER, 2)

  val trainer = RelFreqDiscountingTrainer.instantiate(conf)
  val decoder = FullDecoder.instantiate(conf)

  val hmm = trainer.train(trainCorpus)

  import io.github.ptitjes.hmm.analysis.Results._

  timed("Test HMM") {
    val results = decodeAndCheck(decoder, hmm, devCorpus /*, true*/)
    println(results)
  }
}

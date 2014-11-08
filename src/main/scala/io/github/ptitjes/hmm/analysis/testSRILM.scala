package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.didier.{ParDecoder, RelFreqSRILMTrainer}

object testSRILM extends App {

  val trainCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.dev.encode"))

  val conf = Configuration().set(Trainer.ORDER, 2)

  val trainer = RelFreqSRILMTrainer.instantiate(conf)
  val decoder = ParDecoder.instantiate(conf)

  val hmm = trainer.train(15, trainCorpus)

  import io.github.ptitjes.hmm.analysis.Results._

  timed("Test HMM") {
    val results = decodeAndCheck(decoder, hmm, devCorpus /*, true*/)
    println(results)
  }
}

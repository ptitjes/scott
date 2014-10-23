package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.didier.RelFreqSRILMTrainer
import io.github.ptitjes.hmm.didier.ParDecoder

object testSRILM extends App {

  val trainCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.dev.encode"))

  val trainerConf = Configuration().set(Trainer.ORDER, 2)

  val decoderConf = Configuration()

  val hmm = RelFreqSRILMTrainer.instantiate(trainerConf).train(15, trainCorpus)

  timed("Test HMM") {
    val results = ParDecoder.instantiate(decoderConf).decodeAndCheck(hmm, devCorpus /*, true*/)
    println(results)
  }
}

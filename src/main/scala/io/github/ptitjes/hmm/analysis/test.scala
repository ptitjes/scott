package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._

object test extends App {

  val trainCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.dev.encode"))

  val orderAnalysis = new Analysis {
    def configurations: AnalysisConfigurations =
      AnalysisConfigurations()
        .set(Trainer.ORDER, (1 to 4).toList)

    def trainers: List[Algorithm[Trainer]] = List(
      didier.RelFreqSimpleTrainer,
      didier.RelFreqSRILMTrainer
    )

    def decoders: List[Algorithm[Decoder]] = List(
      didier.ParDecoder
    )
  }

  Analysis run(List(
    orderAnalysis
  ), trainCorpus, devCorpus)
}


package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._

object test extends App {

  val trainCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.dev.encode"))

  val orderAnalysis = new Analysis {
    def configurations: AnalysisConfigurations =
      AnalysisConfigurations()
        .set(Analysis.ALGORITHMS,
          List(
            (didier.RelFreqSimpleTrainer, didier.ParDecoder),
            (didier.RelFreqSRILMTrainer, didier.ParDecoder)
          )
        )
        .set(Trainer.ORDER, (1 to 4).toList)
  }

  val unknownThresholdAnalysis = new Analysis {
    def configurations: AnalysisConfigurations =
      AnalysisConfigurations()
        .set(Analysis.ALGORITHMS,
          List(
            (didier.RelFreqSimpleTrainer, didier.ParDecoder),
            (didier.RelFreqSRILMTrainer, didier.ParDecoder)
          )
        )
        .set(Trainer.ORDER, (1 to 3).toList)
        .set(didier.EmittingTraining.UNKNOWN_THRESHOLD, (1 to 10).toList)
  }

  val results = Analysis.run(List(
    //orderAnalysis,
    unknownThresholdAnalysis
  ), trainCorpus, devCorpus)

  import LaTexReport._

  LaTexReport.generate(results)(
    Graph(unknownThresholdAnalysis,
      didier.EmittingTraining.UNKNOWN_THRESHOLD,
      (1 to 3).toList.map(i => Configuration()
        .set(Analysis.ALGORITHMS, (didier.RelFreqSimpleTrainer, didier.ParDecoder))
        .set(Trainer.ORDER, i)) :::
        (1 to 3).toList.map(i => Configuration()
          .set(Analysis.ALGORITHMS, (didier.RelFreqSRILMTrainer, didier.ParDecoder))
          .set(Trainer.ORDER, i))
    )
  )
}


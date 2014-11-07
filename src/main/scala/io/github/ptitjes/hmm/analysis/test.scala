package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._

object test extends App {

  val trainCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.dev.encode"))

  val orderAnalysis = Analysis()
    .forAll(Analysis.ALGORITHMS,
      (didier.RelFreqSimpleTrainer, didier.ParDecoder),
      (didier.RelFreqSRILMTrainer, didier.ParDecoder)
    )
    .forAll(Trainer.ORDER, 1 to 4)

  val unknownThresholdAnalysis = Analysis()
    .forAll(Analysis.ALGORITHMS,
      (didier.RelFreqSimpleTrainer, didier.ParDecoder),
      (didier.RelFreqSRILMTrainer, didier.ParDecoder)
    )
    .forAll(Trainer.ORDER, 1 to 3)
    .forAll(didier.EmittingTraining.UNKNOWN_THRESHOLD, 1 to 10)

  val results = Analysis.run(trainCorpus, devCorpus,
    List(
      //orderAnalysis,
      unknownThresholdAnalysis
    )
  )

  import LaTexReport._

  LaTexReport.generate(results)(
    Graph(unknownThresholdAnalysis, didier.EmittingTraining.UNKNOWN_THRESHOLD)
  )
}

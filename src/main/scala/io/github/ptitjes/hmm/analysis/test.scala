package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._

object test extends App {

  val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

  val orderAnalysis = Analysis()
    .forAll(Analysis.ALGORITHMS,
      (didier.RelFreqTrainer, didier.FullMTDecoder),
      (didier.RelFreqDiscountingTrainer, didier.FullMTDecoder)
    )
    .forAll(Trainer.ORDER, 1 to 4)

  val interpolatedOrderAnalysis = Analysis()
    .forAll(Analysis.ALGORITHMS,
      (didier.RelFreqDiscountingTrainer, didier.FullMTDecoder)
    )
    .forAll(Trainer.ORDER, 1 to 4)
    .forAll(didier.RelFreqDiscountingTrainer.MULTIPLIER, 1 to 5)

  val interpolatedOrderAnalysisZoom = Analysis()
    .forAll(Analysis.ALGORITHMS,
      (didier.RelFreqDiscountingTrainer, didier.FullMTDecoder)
    )
    .forAll(Trainer.ORDER, 2 to 4)
    .forAll(didier.RelFreqDiscountingTrainer.MULTIPLIER, 2 to 5)

  val interpolatedOrderAnalysisZoomOrder3 = Analysis()
    .forAll(Analysis.ALGORITHMS,
      (didier.RelFreqDiscountingTrainer, didier.FullMTDecoder)
    )
    .forAll(Trainer.ORDER, 2 to 3)
    .forAll(didier.RelFreqDiscountingTrainer.MULTIPLIER, 3 to 10)

  val unknownThresholdAnalysis = Analysis()
    .forAll(Analysis.ALGORITHMS,
      (didier.RelFreqDiscountingTrainer, didier.FullMTDecoder)
    )
    .forAll(Trainer.ORDER, 2 to 3)
    .forAll(didier.RelFreqDiscountingTrainer.MULTIPLIER, 8 to 8)
    .forAll(didier.EmittingTraining.UNKNOWN_THRESHOLD, 1 to 20)

  val results = Analysis.run(trainCorpus, devCorpus,
    List(
      orderAnalysis,
      interpolatedOrderAnalysis,
      interpolatedOrderAnalysisZoom,
      interpolatedOrderAnalysisZoomOrder3,
      unknownThresholdAnalysis
    )
  )

  import LaTexReport._

  val accuracyMeasure = YAxis("Accuracy", "\\%", r => r.accuracy * 100)

  LaTexReport.generate(results)(
    Graph(orderAnalysis, XAxis(Trainer.ORDER), accuracyMeasure),
    Graph(interpolatedOrderAnalysis, XAxis(Trainer.ORDER), accuracyMeasure),
    Graph(interpolatedOrderAnalysisZoom, XAxis(Trainer.ORDER), accuracyMeasure),
    Graph(interpolatedOrderAnalysisZoomOrder3, XAxis(didier.RelFreqDiscountingTrainer.MULTIPLIER), accuracyMeasure),
    Graph(unknownThresholdAnalysis, XAxis(didier.EmittingTraining.UNKNOWN_THRESHOLD), accuracyMeasure)
  )
}

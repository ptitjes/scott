package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._

object test extends App {

  import ConfigurationSet._

  implicit val runner: AnalysisRunner = new AnalysisRunner("report/results.json",
    Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode")),
    Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode")))

  val ratioAnalysis =
    (Analysis.TRAINER forAll didier.RelFreqTrainer and didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullMTDecoder) *
      (Trainer.ORDER from (1 to 3)) *
      (Analysis.CORPUS_RATIO from (10 to 100 by 10))

  val orderAnalysis =
    (Analysis.TRAINER forAll didier.RelFreqTrainer and didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullMTDecoder) *
      (Trainer.ORDER from (1 to 4))

  val interpolatedOrderAnalysis =
    (Analysis.TRAINER as didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullMTDecoder) *
      (Trainer.ORDER from (1 to 4)) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER from (1 to 5))

  val interpolatedOrderAnalysisZoom =
    (Analysis.TRAINER as didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullMTDecoder) *
      (Trainer.ORDER from (2 to 4)) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER from (2 to 5))

  val interpolatedOrderAnalysisZoomOrder3 =
    (Analysis.TRAINER as didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullMTDecoder) *
      (Trainer.ORDER from (2 to 3)) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER from (3 to 10))

  val unknownThresholdAnalysis =
    (Analysis.TRAINER as didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullMTDecoder) *
      (Trainer.ORDER from (2 to 3)) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER as 8) *
      (didier.EmittingTraining.UNKNOWN_THRESHOLD from (1 to 20))

  import LaTexReport._

  val accuracyMeasure = YAxis("Accuracy", "\\%", _.accuracy * 100)
  val unknownAccuracyMeasure = YAxis("Unknown Word Accuracy", "\\%", _.unknownAccuracy * 100)

  LaTexReport.generate(
    Graph(ratioAnalysis, XAxis(Analysis.CORPUS_RATIO), accuracyMeasure),
    Graph(orderAnalysis, XAxis(Trainer.ORDER), accuracyMeasure),
    Graph(interpolatedOrderAnalysis, XAxis(Trainer.ORDER), accuracyMeasure),
    Graph(interpolatedOrderAnalysisZoom, XAxis(Trainer.ORDER), accuracyMeasure),
    Graph(interpolatedOrderAnalysisZoomOrder3, XAxis(didier.RelFreqDiscountingTrainer.MULTIPLIER), accuracyMeasure),
    Graph(unknownThresholdAnalysis, XAxis(didier.EmittingTraining.UNKNOWN_THRESHOLD), accuracyMeasure),
    Graph(unknownThresholdAnalysis, XAxis(didier.EmittingTraining.UNKNOWN_THRESHOLD), unknownAccuracyMeasure)
  )
}

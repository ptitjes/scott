package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._
import io.github.ptitjes.hmm.analysis.LaTexReport._

object test extends App {

  implicit val runner: AnalysisRunner = new AnalysisRunner("report/results.json",
    Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode")),
    Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode")))

  val report: LaTexReport = new LaTexReport("report/report.tex")

  val accuracy = YAxis("Accuracy", "\\%", _.accuracy * 100)
  val unknownAccuracy = YAxis("Unknown Word Accuracy", "\\%", _.unknownAccuracy * 100)

  val `all freq trainers + full decoder` =
    (Analysis.TRAINER forAll didier.RelFreqTrainer and didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullDecoder)

  val `discounting freq trainer + full decoder` =
    (Analysis.TRAINER as didier.RelFreqDiscountingTrainer) *
      (Analysis.DECODER as didier.FullDecoder)

  report << Graph("corpusRatioFull", "Impact de la quantité de corpus d'apprentissage",
    `all freq trainers + full decoder` *
      (Trainer.ORDER from (1 to 3)) *
      (Analysis.CORPUS_RATIO from (10 to 100 by 10)),
    XAxis(Analysis.CORPUS_RATIO), accuracy)

  report << Graph("corpusRatioZoom", "Impact de la quantité de corpus d'apprentissage",
    `all freq trainers + full decoder` *
      (Trainer.ORDER from (1 to 3)) *
      (Analysis.CORPUS_RATIO from (50 to 100 by 10)),
    XAxis(Analysis.CORPUS_RATIO), accuracy)

  report << Graph("orderAnalysis", "Impact de l'ordre",
    `all freq trainers + full decoder` *
      (Trainer.ORDER from (1 to 4)),
    XAxis(Trainer.ORDER), accuracy)

  report << Graph("interpolatedOrderAnalysis", "Impact de l'ordre (avec interpolation)",
    `discounting freq trainer + full decoder` *
      (Trainer.ORDER from (1 to 4)) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER from (1 to 5)),
    XAxis(Trainer.ORDER), accuracy)

  report << Graph("interpolatedOrderAnalysisZoom", "Impact de l'ordre (avec interpolation)",
    `discounting freq trainer + full decoder` *
      (Trainer.ORDER from (2 to 4)) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER from (2 to 10)),
    XAxis(Trainer.ORDER), accuracy)

  report << Graph("interpolatedOrderAnalysisZoomOrder3", "Relativisation de l'interpolation",
    `discounting freq trainer + full decoder` *
      (Trainer.ORDER from (2 to 4)) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER from (3 to 10)),
    XAxis(didier.RelFreqDiscountingTrainer.MULTIPLIER), accuracy)

  val unknownThresholdAnalysis =
    (((Analysis.TRAINER as didier.RelFreqDiscountingTrainer) *
      (didier.RelFreqDiscountingTrainer.MULTIPLIER as 8) * (Trainer.ORDER from (2 to 3))) +
      ((Analysis.TRAINER as didier.RelFreqTrainer) * (Trainer.ORDER as 2))) *
      (Analysis.DECODER as didier.FullDecoder) *
      (didier.EmittingTraining.UNKNOWN_THRESHOLD from (1 to 20))

  report << Graph("unknownGlobalAccuracy", "Impact du seuil de mot inconnu",
    unknownThresholdAnalysis,
    XAxis(didier.EmittingTraining.UNKNOWN_THRESHOLD), accuracy)

  report << Graph("unknownUnknownAccuracy", "Impact du seuil de mot inconnu",
    unknownThresholdAnalysis,
    XAxis(didier.EmittingTraining.UNKNOWN_THRESHOLD), unknownAccuracy)

  report.generate
}

package io.github.ptitjes.hmm.scripts

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._
import io.github.ptitjes.hmm.analysis.LaTexReport._
import io.github.ptitjes.hmm.analysis.{ConfigurationSet, Analysis, AnalysisRunner, LaTexReport}
import io.github.ptitjes.hmm.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.hmm.trainers.{EmittingTraining, RelFreqDiscountingTrainer, RelFreqTrainer}

object analyseGenerative extends App {

	implicit val runner: AnalysisRunner = new AnalysisRunner("analysis/results-generative.json",
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS),
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS))

	val report: LaTexReport = new LaTexReport("report/report-generative.tex")

	val accuracy = YAxis("Accuracy", "\\%", (_: Int, r) => r.accuracy * 100)
	val unknownAccuracy = YAxis("Unknown Word Accuracy", "\\%", (_: Int, r) => r.unknownAccuracy * 100)

	val `all freq trainers + all decoders` =
		(Configuration.TRAINER forAll RelFreqTrainer and RelFreqDiscountingTrainer) *
			(Configuration.DECODER forAll FullDecoder and BeamDecoder)

	val `discounting freq trainer + full and beam decoder` =
		(Configuration.TRAINER as trainers.RelFreqDiscountingTrainer) *
			(Configuration.DECODER as decoders.FullDecoder)

	report << LinePlot("corpusRatioFull", "Impact de la quantité de corpus d'apprentissage",
		ConfigurationSet(),
		`all freq trainers + all decoders` *
			(Trainer.ORDER from (1 to 3)),
		Configuration.CORPUS_RATIO from (10 to 100 by 10), accuracy)

	report << LinePlot("corpusRatioZoom", "Impact de la quantité de corpus d'apprentissage",
		ConfigurationSet(),
		`all freq trainers + all decoders` *
			(Trainer.ORDER from (1 to 3)),
		Configuration.CORPUS_RATIO from (50 to 100 by 10), accuracy)

	report << LinePlot("orderAnalysis", "Impact de l'ordre",
		ConfigurationSet(),
		`all freq trainers + all decoders`,
		Trainer.ORDER from (1 to 4), accuracy)

	report << LinePlot("interpolatedOrderAnalysis", "Impact de l'ordre (avec interpolation)",
		ConfigurationSet(),
		`discounting freq trainer + full and beam decoder` *
			(trainers.RelFreqDiscountingTrainer.MULTIPLIER from (1 to 5)),
		Trainer.ORDER from (1 to 4), accuracy)

	report << LinePlot("interpolatedOrderAnalysisZoom", "Impact de l'ordre (avec interpolation)",
		ConfigurationSet(),
		`discounting freq trainer + full and beam decoder` *
			(trainers.RelFreqDiscountingTrainer.MULTIPLIER from (2 to 10)),
		Trainer.ORDER from (2 to 4), accuracy)

	report << LinePlot("interpolatedOrderAnalysisZoomOrderThree", "Relativisation de l'interpolation",
		ConfigurationSet(),
		`discounting freq trainer + full and beam decoder` *
			(Trainer.ORDER from (2 to 4)),
		RelFreqDiscountingTrainer.MULTIPLIER from (3 to 10), accuracy)

	val unknownThresholdAnalysis =
		(Configuration.TRAINER as RelFreqDiscountingTrainer) * (Trainer.ORDER from (2 to 3)) +
			(Configuration.TRAINER as RelFreqTrainer) * (Trainer.ORDER as 2)

	report << LinePlot("unknownGlobalAccuracy", "Impact du seuil de mot inconnu",
		Configuration.DECODER as BeamDecoder,
		unknownThresholdAnalysis,
		EmittingTraining.UNKNOWN_THRESHOLD from (1 to 20), accuracy)

	report << LinePlot("unknownUnknownAccuracy", "Impact du seuil de mot inconnu",
		Configuration.DECODER as BeamDecoder,
		unknownThresholdAnalysis,
		EmittingTraining.UNKNOWN_THRESHOLD from (1 to 20), unknownAccuracy)

	report << BarPlot("accuracyPerCategory", "Impact du seuil de mot inconnu",
		Configuration.DECODER as BeamDecoder,
		unknownThresholdAnalysis,
		(0 until 15).toList, YAxis("Accuracy", "\\%", (t: Int, r) => r.perCategoryCounts(t).accuracy * 100))

	report.generate
}

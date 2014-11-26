package io.github.ptitjes.hmm.scripts

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._
import io.github.ptitjes.hmm.analysis.LaTexReport._
import io.github.ptitjes.hmm.analysis.{Analysis, AnalysisRunner, LaTexReport}
import io.github.ptitjes.hmm.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.hmm.trainers.{EmittingTraining, RelFreqDiscountingTrainer, RelFreqTrainer}

object analyseGenerative extends App {

	implicit val runner: AnalysisRunner = new AnalysisRunner("analysis/results-generative.json",
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS),
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS))

	val report: LaTexReport = new LaTexReport("report/report-generative.tex")

	val accuracy = YAxis("Accuracy", "\\%", _.accuracy * 100)
	val unknownAccuracy = YAxis("Unknown Word Accuracy", "\\%", _.unknownAccuracy * 100)

	val `all freq trainers + all decoders` =
		(Configuration.TRAINER forAll RelFreqTrainer and RelFreqDiscountingTrainer) *
			(Configuration.DECODER forAll FullDecoder and BeamDecoder)

	val `discounting freq trainer + full and beam decoder` =
		(Configuration.TRAINER as trainers.RelFreqDiscountingTrainer) *
			(Configuration.DECODER as decoders.FullDecoder)

	report << Graph("corpusRatioFull", "Impact de la quantité de corpus d'apprentissage",
		`all freq trainers + all decoders` *
			(Trainer.ORDER from (1 to 3)) *
			(Configuration.CORPUS_RATIO from (10 to 100 by 10)),
		XAxis(Configuration.CORPUS_RATIO), accuracy)

	report << Graph("corpusRatioZoom", "Impact de la quantité de corpus d'apprentissage",
		`all freq trainers + all decoders` *
			(Trainer.ORDER from (1 to 3)) *
			(Configuration.CORPUS_RATIO from (50 to 100 by 10)),
		XAxis(Configuration.CORPUS_RATIO), accuracy)

	report << Graph("orderAnalysis", "Impact de l'ordre",
		`all freq trainers + all decoders` *
			(Trainer.ORDER from (1 to 4)),
		XAxis(Trainer.ORDER), accuracy)

	report << Graph("interpolatedOrderAnalysis", "Impact de l'ordre (avec interpolation)",
		`discounting freq trainer + full and beam decoder` *
			(Trainer.ORDER from (1 to 4)) *
			(trainers.RelFreqDiscountingTrainer.MULTIPLIER from (1 to 5)),
		XAxis(Trainer.ORDER), accuracy)

	report << Graph("interpolatedOrderAnalysisZoom", "Impact de l'ordre (avec interpolation)",
		`discounting freq trainer + full and beam decoder` *
			(Trainer.ORDER from (2 to 4)) *
			(trainers.RelFreqDiscountingTrainer.MULTIPLIER from (2 to 10)),
		XAxis(Trainer.ORDER), accuracy)

	report << Graph("interpolatedOrderAnalysisZoomOrder3", "Relativisation de l'interpolation",
		`discounting freq trainer + full and beam decoder` *
			(Trainer.ORDER from (2 to 4)) *
			(trainers.RelFreqDiscountingTrainer.MULTIPLIER from (3 to 10)),
		XAxis(trainers.RelFreqDiscountingTrainer.MULTIPLIER), accuracy)

	val unknownThresholdAnalysis =
		(((Configuration.TRAINER as trainers.RelFreqDiscountingTrainer) *
			(trainers.RelFreqDiscountingTrainer.MULTIPLIER as 8) * (Trainer.ORDER from (2 to 3))) +
			((Configuration.TRAINER as trainers.RelFreqTrainer) * (Trainer.ORDER as 2))) *
			(Configuration.DECODER as decoders.FullDecoder) *
			(EmittingTraining.UNKNOWN_THRESHOLD from (1 to 20))

	report << Graph("unknownGlobalAccuracy", "Impact du seuil de mot inconnu",
		unknownThresholdAnalysis,
		XAxis(trainers.EmittingTraining.UNKNOWN_THRESHOLD), accuracy)

	report << Graph("unknownUnknownAccuracy", "Impact du seuil de mot inconnu",
		unknownThresholdAnalysis,
		XAxis(trainers.EmittingTraining.UNKNOWN_THRESHOLD), unknownAccuracy)

	report.generate
}

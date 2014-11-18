package io.github.ptitjes.hmm.scripts

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._
import io.github.ptitjes.hmm.analysis.LaTexReport._
import io.github.ptitjes.hmm.analysis.{Analysis, AnalysisRunner, LaTexReport}
import io.github.ptitjes.hmm.decoders.FullDecoder
import io.github.ptitjes.hmm.trainers.DiscriminantTrainer

object analyseDiscriminant extends App {

	implicit val runner: AnalysisRunner = new AnalysisRunner("report/results-discriminant.json",
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS),
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS))

	val report: LaTexReport = new LaTexReport("report/report-discriminant.tex")

	val accuracy = YAxis("Accuracy", "\\%", _.accuracy * 100)
	val unknownAccuracy = YAxis("Unknown Word Accuracy", "\\%", _.unknownAccuracy * 100)

	val `disc trainer + full decoder` =
		(Configuration.TRAINER as trainers.DiscriminantTrainer) *
			(Configuration.DECODER as FullDecoder)

	report << Graph("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`disc trainer + full decoder` *
			(Trainer.ORDER from (1 to 2)) *
			(DiscriminantTrainer.ITERATION_COUNT forAll 1 and 5 and 10 and 15 and 20),
		XAxis(trainers.DiscriminantTrainer.ITERATION_COUNT), accuracy)

	report << Graph("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`disc trainer + full decoder` *
			(Trainer.ORDER from (1 to 2)) *
			(DiscriminantTrainer.ITERATION_COUNT forAll 1 and 5 and 10 and 15 and 20),
		XAxis(trainers.DiscriminantTrainer.ITERATION_COUNT), unknownAccuracy)

	report.generate
}

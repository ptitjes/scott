package io.github.ptitjes.hmm.scripts

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._
import io.github.ptitjes.hmm.analysis.LaTexReport._
import io.github.ptitjes.hmm.analysis.{AnalysisRunner, LaTexReport}
import io.github.ptitjes.hmm.decoders.FullDecoder
import io.github.ptitjes.hmm.trainers.DiscriminantTrainer
import io.github.ptitjes.hmm.trainers.DiscriminantTrainer._

object analyseDiscriminant extends App {

	implicit val runner: AnalysisRunner = new AnalysisRunner("analysis/results-discriminant.json",
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS),
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS))

	val report: LaTexReport = new LaTexReport("report/report-discriminant.tex")

	val accuracy = YAxis("Accuracy", "\\%", _.accuracy * 100)
	val unknownAccuracy = YAxis("Unknown Word Accuracy", "\\%", _.unknownAccuracy * 100)

	val `disc trainer + full decoder` =
		(Configuration.TRAINER as DiscriminantTrainer) * (Configuration.DECODER as FullDecoder)

	val `all orders` = `disc trainer + full decoder` * (Trainer.ORDER from (1 to 2))

	val `all averaging strategies` = `all orders` *
		(AVERAGING forAll NO_AVERAGING and PARTIAL_AVERAGING and COMPLETE_AVERAGING)

	val maxIterations = 30

	runner.resultsFor(`all averaging strategies` * (DiscriminantTrainer.ITERATION_COUNT as maxIterations))

	report << Graph("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`all averaging strategies` * (ITERATION_COUNT from (1 to maxIterations)),
		XAxis(ITERATION_COUNT), accuracy)

	report << Graph("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`all orders` * (ITERATION_COUNT from (4 to maxIterations)) *
			(AVERAGING forAll PARTIAL_AVERAGING and COMPLETE_AVERAGING),
		XAxis(ITERATION_COUNT), accuracy)

	report << Graph("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`all averaging strategies` * (ITERATION_COUNT from (1 to maxIterations)),
		XAxis(ITERATION_COUNT), unknownAccuracy)

	report.generate
}

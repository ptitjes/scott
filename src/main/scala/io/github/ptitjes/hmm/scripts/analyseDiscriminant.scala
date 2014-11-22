package io.github.ptitjes.hmm.scripts

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._
import io.github.ptitjes.hmm.analysis.LaTexReport._
import io.github.ptitjes.hmm.analysis.{Analysis, AnalysisRunner, LaTexReport}
import io.github.ptitjes.hmm.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.hmm.trainers.DiscriminantTrainer

object analyseDiscriminant extends App {

	implicit val runner: AnalysisRunner = new AnalysisRunner("results/results-discriminant.json",
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS),
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS))

	val report: LaTexReport = new LaTexReport("report/report-discriminant.tex")

	val accuracy = YAxis("Accuracy", "\\%", _.accuracy * 100)
	val unknownAccuracy = YAxis("Unknown Word Accuracy", "\\%", _.unknownAccuracy * 100)

	val `disc trainer + full decoder` =
		(Configuration.TRAINER as trainers.DiscriminantTrainer) *
			(Configuration.DECODER as FullDecoder)

	val all = `disc trainer + full decoder` *
		(Trainer.ORDER from (1 to 2)) *
		(DiscriminantTrainer.AVERAGING forAll
			DiscriminantTrainer.NO_AVERAGING and
			DiscriminantTrainer.PARTIAL_AVERAGING and
			DiscriminantTrainer.COMPLETE_AVERAGING)

	val maxIterations = 30

	runner.resultsFor(all * (DiscriminantTrainer.ITERATION_COUNT as maxIterations))

	report << Graph("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		all * (DiscriminantTrainer.ITERATION_COUNT from (1 to maxIterations)),
		XAxis(trainers.DiscriminantTrainer.ITERATION_COUNT), accuracy)

	report << Graph("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		all * (DiscriminantTrainer.ITERATION_COUNT from (1 to maxIterations)),
		XAxis(trainers.DiscriminantTrainer.ITERATION_COUNT), unknownAccuracy)

	report.generate
}

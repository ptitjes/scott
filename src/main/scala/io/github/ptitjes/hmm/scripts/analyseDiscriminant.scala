package io.github.ptitjes.hmm.scripts

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._
import io.github.ptitjes.hmm.analysis.LaTexReport._
import io.github.ptitjes.hmm.analysis.{AnalysisRunner, LaTexReport}
import io.github.ptitjes.hmm.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.hmm.trainers.DiscriminantTrainer
import io.github.ptitjes.hmm.trainers.DiscriminantTrainer._

object analyseDiscriminant extends App {

	implicit val runner: AnalysisRunner = new AnalysisRunner("analysis/results-discriminant.json",
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS),
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS))

	val report: LaTexReport = new LaTexReport("report/report-discriminant.tex")

	val accuracy = YAxis("Accuracy", "\\%", (_: Int, r) => r.accuracy * 100)
	val unknownAccuracy = YAxis("Unknown Word Accuracy", "\\%", (_: Int, r) => r.unknownAccuracy * 100)

	val `perceptron trainer` =
		(Configuration.TRAINER as DiscriminantTrainer) * (DiscriminantTrainer.DECODER as FullDecoder)

	val `all orders` = Trainer.ORDER from (1 to 2)

	val `all averaging strategies` =
		AVERAGING forAll NO_AVERAGING and PARTIAL_AVERAGING and COMPLETE_AVERAGING

	val maxIterations = 30

	val `all decoders` = Configuration.DECODER forAll FullDecoder and BeamDecoder

	runner.computeResults(
		`perceptron trainer` *
			`all orders` *
			`all averaging strategies` *
			(DiscriminantTrainer.ITERATION_COUNT as maxIterations) *
			`all decoders`
	)

	report << LinePlot("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`perceptron trainer`,
		`all orders` * `all averaging strategies` * `all decoders`,
		ITERATION_COUNT from (1 to maxIterations), accuracy)

	report << LinePlot("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`perceptron trainer`,
		`all orders` * `all decoders` *
			(AVERAGING forAll PARTIAL_AVERAGING and COMPLETE_AVERAGING),
		ITERATION_COUNT from (1 to maxIterations), accuracy)

	report << LinePlot("discriminant", "Impact du nombre d'itérations sur la méthode discriminant",
		`perceptron trainer`,
		`all orders` * `all averaging strategies` * `all decoders`,
		ITERATION_COUNT from (1 to maxIterations), unknownAccuracy)

	report.generate
}

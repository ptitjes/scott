package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Results._
import io.github.ptitjes.hmm.analysis._
import io.github.ptitjes.hmm.analysis.ConfigurationSet._

object testDiscriminant extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

	val conf = Configuration()
		.set(Trainer.ORDER, 2)
		.set(DiscriminantTrainer.ITERATION_COUNT, 1)

	val trainer = DiscriminantTrainer.instantiate(conf)
	val decoder = FullDecoder.instantiate(conf)

	println(conf + "\n" + trainDecodeAndCheck(trainer, decoder, trainCorpus, devCorpus, debug = true))

	//	implicit val runner: AnalysisRunner = new AnalysisRunner("report/results-discriminant.json",
	//		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode")),
	//		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode")),
	//		force = true
	//	)
	//
	//	val iterationAnalysis =
	//		(Analysis.TRAINER forAll didier.DiscriminantTrainer) *
	//			(Analysis.DECODER as didier.FullDecoder) *
	//			(Trainer.ORDER from (1 to 1)) *
	//			(DiscriminantTrainer.ITERATION_COUNT from (1 to 1)) *
	//			(DiscriminantTrainer.FEATURES forAll false and true)
	//
	//	runner.resultsFor(iterationAnalysis)
}

package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Analysis
import io.github.ptitjes.hmm.analysis.Results._

object testDiscriminant extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)

	val conf = Configuration()
		.set(Analysis.TRAINER, DiscriminantTrainer)
		.set(Trainer.ORDER, 2)
		//		.set(DiscriminantTrainer.DECODER, FullDecoder)
		.set(DiscriminantTrainer.ITERATION_COUNT, 15)
		.set(Analysis.DECODER, BeamDecoder)
		.set(BeamDecoder.BEAM, 5)

	trainDecodeAndCheck(conf, trainCorpus, devCorpus, debug = false).display()
	println(conf)
}

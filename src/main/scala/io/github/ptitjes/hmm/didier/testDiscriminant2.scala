package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Results._

object testDiscriminant2 extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)

	val conf = Configuration()
		.set(Trainer.ORDER, 2)
		.set(DiscriminantTrainer.ITERATION_COUNT, 15)

	val trainer = DiscriminantTrainer.instantiate(conf)
	val decoder = FullDecoder.instantiate(conf)

	trainDecodeAndCheck(trainer, decoder, trainCorpus, devCorpus, debug = true).display()
	println(conf)
}

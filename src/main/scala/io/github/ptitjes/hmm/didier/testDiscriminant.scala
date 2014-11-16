package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Results._

object testDiscriminant extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

	val conf = Configuration()
		.set(Trainer.ORDER, 2)
		.set(DiscriminantTrainer.ITERATION_COUNT, 1)

	val trainer = DiscriminantTrainer.instantiate(conf)
	val decoder = FullDecoder.instantiate(conf)

	trainDecodeAndCheck(trainer, decoder, trainCorpus, devCorpus, debug = false).display()
	println(conf)
}

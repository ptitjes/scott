package io.github.ptitjes.hmm.didier

import java.io.File

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Results._

object testRelFreq extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))
	val testCorpus = Corpora.annotatedFrom(new File("/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"))

	val conf = Configuration()
		.set(Trainer.ORDER, 2)
    .set(EmittingTraining.UNKNOWN_THRESHOLD, 18)

	val trainer = RelFreqTrainer.instantiate(conf)
	val decoder = FullDecoder.instantiate(conf)

	//trainDecodeAndCheck(trainer, decoder, trainCorpus, trainCorpus).display()
	trainDecodeAndCheck(trainer, decoder, trainCorpus, devCorpus, debug = false).display()
	trainDecodeAndCheck(trainer, decoder, trainCorpus, testCorpus, debug = false).display()
	trainDecodeAndCheck(trainer, decoder, Corpora.merge(trainCorpus, devCorpus), testCorpus, debug = false).display()
}

package io.github.ptitjes.hmm.didier

import java.io.File

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.analysis.Results._

object testIO extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

	val conf = Configuration()
		.set(Trainer.ORDER, 1)
		.set(DiscriminantTrainer.ITERATION_COUNT, 1)

	val trainer = DiscriminantTrainer.instantiate(conf)
	val decoder = FullDecoder.instantiate(conf)

	val (hmm, trainingElapsedTime) = timed {
		trainer.train(trainCorpus)
	}

	val (hypCorpus, decodingElapsedTime) = timed {
		decoder.setHmm(hmm)
		decoder.decode(devCorpus)
	}

	println(check(trainCorpus, devCorpus, hmm, hypCorpus,
		trainingElapsedTime, decodingElapsedTime))

	toFile(hmm, new File("temp/hmm.json"))
	val hmmFromFile = fromFile(new File("temp/hmm.json"))

	val (hypCorpusFromFile, decodingElapsedTimeFromFile) = timed {
		decoder.setHmm(hmmFromFile)
		decoder.decode(devCorpus)
	}

	println(check(trainCorpus, devCorpus, hmmFromFile, hypCorpusFromFile,
		trainingElapsedTime, decodingElapsedTimeFromFile))
}

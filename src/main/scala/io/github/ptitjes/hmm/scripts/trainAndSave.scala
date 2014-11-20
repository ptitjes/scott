package io.github.ptitjes.hmm.scripts

import java.io.{File, FileWriter, PrintWriter}

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.{Checking, Analysis}
import io.github.ptitjes.hmm.analysis.Results._
import io.github.ptitjes.hmm.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.hmm.trainers.{RelFreqTrainer, DiscriminantTrainer}

object trainAndSave extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
	private val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"
	val testCorpus = Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS)

	val conf = Configuration()
		//		.set(Configuration.TRAINER, RelFreqTrainer)
		.set(Configuration.TRAINER, DiscriminantTrainer)
		.set(Trainer.ORDER, 2)
		//		.set(DiscriminantTrainer.DECODER, FullDecoder)
		.set(DiscriminantTrainer.ITERATION_COUNT, 15)
		.set(DiscriminantTrainer.AVERAGING, DiscriminantTrainer.COMPLETE_AVERAGING)
		//		.set(Configuration.DECODER, FullDecoder)
		.set(Configuration.DECODER, BeamDecoder)
		.set(BeamDecoder.BEAM, 5)

	println(conf)

	val trainer = conf(Configuration.TRAINER).instantiate(conf)
	val (hmm, trainingElapsedTime) = timed {
		trainer.train(trainCorpus)
	}

	val decoder = conf(Configuration.DECODER).instantiate(hmm, conf)
	val (hypCorpus, decodingElapsedTime) = timed {
		decoder.decode(devCorpus)
	}

	//toFile(hmm, new File("temp/" + conf.toFilename + ".json"))

	val results = Checking.check(conf, hmm, devCorpus, hypCorpus,
		trainingElapsedTime, decodingElapsedTime,
		new File("temp/" + conf.toFilename + ".check"))

	results.display()
}

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
		.set(DiscriminantTrainer.ITERATION_COUNT, 14)
		.set(DiscriminantTrainer.AVERAGING, DiscriminantTrainer.COMPLETE_AVERAGING)
		.set(Configuration.DECODER, BeamDecoder)

	println(conf)

	val trainer = conf(Configuration.TRAINER).instantiate(conf)
	val (hmm, trainingElapsedTime) = timed {
		trainer.train(trainCorpus)
	}

	private val hmmFilename = "temp/" + conf.toFilename + ".json"
	timed(s"Saving '$hmmFilename'") {
		toFile(hmm, new File(hmmFilename))
	}

	decode(conf)

	def decode(configuration: Configuration) {
		val decoder = configuration(Configuration.DECODER).instantiate(hmm, configuration)
		val (hypCorpus, decodingElapsedTime) = timed {
			decoder.decode(devCorpus)
		}

		val results = Checking.check(configuration, hmm, devCorpus, hypCorpus,
			trainingElapsedTime, decodingElapsedTime,
			new File("temp/" + configuration.toFilename + ".check"))

		results.display()
	}
}

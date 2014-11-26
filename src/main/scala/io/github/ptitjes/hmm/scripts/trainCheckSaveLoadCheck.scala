package io.github.ptitjes.hmm.scripts

import java.io.{File, FileWriter, PrintWriter}

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.{Checking, Analysis}
import io.github.ptitjes.hmm.analysis.Results._
import io.github.ptitjes.hmm.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.hmm.trainers.{RelFreqTrainer, DiscriminantTrainer}

object trainCheckSaveLoadCheck extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
	private val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"
	val testCorpus = Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS)

	val conf = Configuration()
		.set(Configuration.TRAINER, DiscriminantTrainer)
		.set(Trainer.ORDER, 2)
		.set(DiscriminantTrainer.DECODER, FullDecoder)
		.set(DiscriminantTrainer.ITERATION_COUNT, 1)
		.set(DiscriminantTrainer.AVERAGING, DiscriminantTrainer.NO_AVERAGING)
		.set(Configuration.DECODER, BeamDecoder)

	val trainingConf = conf.completeForTraining
	val decodingConf = conf.completeForDecoding

	println("Training with " + trainingConf)

	val trainer = trainingConf(Configuration.TRAINER).instantiate(trainingConf)
	val (hmm, trainingElapsedTime) = timed {
		trainer.train(trainCorpus)
	}

	private val hmmFilename = "temp/" + trainingConf.toFilename + ".json"
	timed(s"Saving '$hmmFilename'") {
		toFile(hmm, new File(hmmFilename))
	}

	println("Decoding with " + decodingConf + " on " + trainingConf)
	decode(hmm, decodingConf, trainingConf)

	val (hmm2, loadTime) = timed(s"Loading '$hmmFilename'") {
		fromFile(new File(hmmFilename))
	}

	println("Decoding with " + decodingConf + " on " + trainingConf)
	decode(hmm2, decodingConf, trainingConf)

	def decode(hmm: HiddenMarkovModel, decodingConf: Configuration, trainingConf: Configuration) {
		val decoder = decodingConf(Configuration.DECODER).instantiate(hmm, decodingConf)
		val (hypCorpus, decodingElapsedTime) = timed {
			decoder.decode(devCorpus)
		}

		val results = Checking.check(decodingConf, hmm, devCorpus, hypCorpus,
			new File("temp/" + decodingConf.toFilename + "-on-" + trainingConf.toFilename + ".check"))

		results.display()
	}
}

package io.github.ptitjes.scott.scripts

import java.io.{File, FileWriter, PrintWriter}

import io.github.ptitjes.scott.HiddenMarkovModel._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.analysis.{Checking, Analysis}
import io.github.ptitjes.scott.analysis.Results._
import io.github.ptitjes.scott.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.scott.trainers.{RelFreqTrainer, DiscriminantTrainer}

object trainCheckSaveLoadCheck extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)

	val conf = Configuration()
		.set(Configuration.TRAINER, DiscriminantTrainer)
		.set(Trainer.ORDER, 2)
		.set(DiscriminantTrainer.DECODER, FullDecoder)
		.set(DiscriminantTrainer.ITERATION_COUNT, 10)
		.set(DiscriminantTrainer.AVERAGING, DiscriminantTrainer.COMPLETE_AVERAGING)
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
		val hypCorpus = decoder.decode(devCorpus)

		val results = Checking.check(decodingConf, hmm, devCorpus, hypCorpus,
			new File("temp/" + decodingConf.toFilename + "-on-" + trainingConf.toFilename + ".check"))

		results.display()
	}
}

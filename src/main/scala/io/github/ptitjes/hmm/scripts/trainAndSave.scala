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
		.set(Trainer.ORDER, 1)
//		.set(DiscriminantTrainer.DECODER, FullDecoder)
		.set(DiscriminantTrainer.ITERATION_COUNT, 1)
		.set(DiscriminantTrainer.AVERAGING, DiscriminantTrainer.NO_AVERAGING)
		//				.set(Configuration.DECODER, FullDecoder)
		.set(Configuration.DECODER, BeamDecoder)
		.set(BeamDecoder.BEAM, 5)

	println(conf)

	val trainer = conf(Configuration.TRAINER).instantiate(conf)
	val (hmm, trainingElapsedTime) = timed {
		trainer.train(trainCorpus)
	}

	decode(conf)

//	val conf2 = Configuration()
//		.set(Configuration.DECODER, FullDecoder)
//
//	decodeAndSave(conf2)

	toFile(hmm, new File("temp/" + conf.toFilename + ".json"))

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

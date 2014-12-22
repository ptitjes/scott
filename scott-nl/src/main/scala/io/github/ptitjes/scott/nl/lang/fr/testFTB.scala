package io.github.ptitjes.scott.nl.lang.fr

import java.io.File

import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.analysis.Checking
import io.github.ptitjes.scott.corpora.Lexica
import io.github.ptitjes.scott.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.scott.nl.conll.CoNLLXParser
import io.github.ptitjes.scott.trainers.DiscriminantTrainer

import scala.io.Source

object testFTB extends App {

	val PATH_TO_FTB = "/home/didier/Documents/Work/Master/DM/InfStat/ftb"
	val CONLL_CORPUS = "ftb4+mc+undocpd+fct+structmod110908"

	val trainCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_1.dep_conll"
	val devCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_2.dep_conll"
	val testCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_3.dep_conll"

	val parser = new CoNLLXParser
	val profile = FTB.BasicCoNLLProfile
	println(profile.tagSet.tags.size + " categories")
	println(profile.tagSet.tags.mkString("\n"))
	val trainCorpus = parser.parse(profile, Source.fromFile(trainCorpusPath), Lexica.WORDS)
	val devCorpus = parser.parse(profile, Source.fromFile(devCorpusPath), Lexica.WORDS)
	val testCorpus = parser.parse(profile, Source.fromFile(testCorpusPath), Lexica.WORDS)

	val conf = Configuration()
		.set(Configuration.TRAINER, DiscriminantTrainer)
		.set(Trainer.ORDER, 2)
		.set(DiscriminantTrainer.DECODER, FullDecoder)
		.set(DiscriminantTrainer.ITERATION_COUNT, 30)
		.set(DiscriminantTrainer.AVERAGING, DiscriminantTrainer.COMPLETE_AVERAGING)
		.set(Configuration.DECODER, BeamDecoder)

	val trainingConf = conf.completeForTraining
	val decodingConf = conf.completeForDecoding

	println("Training with " + trainingConf)

	val trainer = trainingConf(Configuration.TRAINER).instantiate(trainingConf).asInstanceOf[IterativeTrainer]
	val (hmm, trainingElapsedTime) = timed {
		trainer.train(trainCorpus, new IterationCallback {
			override def iterationDone(configuration: Configuration, hmm: HiddenMarkovModel, elapsedTime: Long): Unit = {
				println("Decoding with " + decodingConf + " on " + configuration)
				decode(hmm, decodingConf, configuration)
			}
		})
	}

	def decode(hmm: HiddenMarkovModel, decodingConf: Configuration, trainingConf: Configuration) {
		val decoder = decodingConf(Configuration.DECODER).instantiate(hmm, decodingConf)
		val hypCorpus = decoder.decode(devCorpus)

		val results = Checking.check(decodingConf, hmm, devCorpus, hypCorpus, profile.tagSet,
			new File("temp/" + decodingConf.toFilename + "-on-" + trainingConf.toFilename + ".check"))

		results.display()
	}
}

package io.github.ptitjes.hmm.didier

import java.io.{File, FileWriter, PrintWriter}

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Analysis
import io.github.ptitjes.hmm.analysis.Results._

object testTrainAndSave extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
	private val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"
	val testCorpus = Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS)

	val conf = Configuration()
		.set(Analysis.TRAINER, DiscriminantTrainer)
		.set(Trainer.ORDER, 2)
		//		.set(DiscriminantTrainer.DECODER, FullDecoder)
		.set(DiscriminantTrainer.ITERATION_COUNT, 40)
		.set(DiscriminantTrainer.AVERAGING, true)
		.set(Analysis.DECODER, FullDecoder)
	//		.set(Analysis.DECODER, BeamDecoder)
	//		.set(BeamDecoder.BEAM, 5)

	println(conf)

	val (hmm, trainingElapsedTime) = timed {
		val trainer = conf(Analysis.TRAINER).instantiate(conf)
		trainer.train(trainCorpus)
	}

	val (hypCorpus, decodingElapsedTime) = timed {
		val decoder = conf(Analysis.DECODER).instantiate(conf)
		decoder.setHmm(hmm)
		decoder.decode(devCorpus)
	}

	toFile(hmm, new File("temp/" + conf.toFilename + ".json"))

	val results = using(new FileWriter(new File("temp/" + conf.toFilename + ".check"))) {
		fileOutput => using(new PrintWriter(fileOutput)) {
			out =>
				out.println(conf)
				out.println()

				check(hmm, devCorpus, hypCorpus,
					trainingElapsedTime, decodingElapsedTime, true, out)
		}
	}

	results.display()
}

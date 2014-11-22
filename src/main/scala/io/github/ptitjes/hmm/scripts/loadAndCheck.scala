package io.github.ptitjes.hmm.scripts

import java.io.File

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Checking
import io.github.ptitjes.hmm.decoders._

object loadAndCheck extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
	private val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"
	val testCorpus = Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS)

	val hmmFilename = "selected-hmms/Disc-Full-Averaging-Complete-Iterations-14-Order-2.json"

	val conf = Configuration()
		.set(Configuration.DECODER, BeamDecoder)

	val (hmm, loadTime) = timed(s"Loading '$hmmFilename'") {
		fromFile(new File(hmmFilename))
	}

	val decoder = conf(Configuration.DECODER).instantiate(hmm, conf)
	val (hypCorpus, decodingElapsedTime) = timed {
		decoder.decode(devCorpus)
	}

	val results = Checking.check(conf, hmm, devCorpus, hypCorpus, loadTime, decodingElapsedTime,
		new File("temp/" + conf.toFilename + ".check"))
	results.display()
}

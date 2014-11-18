package io.github.ptitjes.hmm.scripts

import java.io.File

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Analysis
import io.github.ptitjes.hmm.analysis.Results._
import io.github.ptitjes.hmm.decoders.FullDecoder

object loadAndCheck extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
	private val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"
	val testCorpus = Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS)

	val hmmFilename = "selected-hmms/Disc-Full-Averaging-Complete-Iterations-40-Order-2.json"

	val conf = Configuration()
		.set(Configuration.DECODER, FullDecoder)
	//		.set(Configuration.DECODER, BeamDecoder)
	//		.set(BeamDecoder.BEAM, 5)

	val (hmm, loadTime) = timed {
		fromFile(new File(hmmFilename))
	}

	val decoder = conf(Configuration.DECODER).instantiate(hmm, conf)
	val (hypCorpus, decodingElapsedTime) = timed {
		decoder.decode(devCorpus)
	}

	val results = check(hmm, devCorpus, hypCorpus, loadTime, decodingElapsedTime)
	results.display()
}

package io.github.ptitjes.hmm.scripts

import java.io.File

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Checking
import io.github.ptitjes.hmm.decoders._

object loadAndCheck extends App {

	val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val testCorpus = {
		//				Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS)
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
	}

	val useBeam = true
	val (hmmDirname, hmmName) = {
		("analysis/hmms/", "Perceptron-Full-Averaging=Complete-Corpus-Ratio=100-Features=Base-Iterations=10-Order=2")
		//		("selected-hmms/", "Disc-Full-Averaging-Complete-Iterations-14-Order-2")
	}

	val hmmFilename = hmmDirname + hmmName + ".json"
	val conf = Configuration().set(Configuration.DECODER, if (useBeam) BeamDecoder else FullDecoder)

	val (hmm, loadTime) = timed(s"Loading '$hmmFilename'") {
		fromFile(new File(hmmFilename))
	}

	val decoder = conf(Configuration.DECODER).instantiate(hmm, conf)
	val hypCorpus = decoder.decode(testCorpus)

	val results = Checking.check(conf, hmm, testCorpus, hypCorpus,
		new File("temp/" + conf.toFilename + "-on-" + hmmName + ".check"))
	results.display()
}

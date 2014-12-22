package io.github.ptitjes.scott.scripts

import java.io.File

import io.github.ptitjes.scott.corpora.Corpora
import Corpora._
import io.github.ptitjes.scott.HiddenMarkovModel._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.analysis.Checking
import io.github.ptitjes.scott.decoders._

object loadAndCheck extends App {

	val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	val testCorpus = {
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
//						Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS)
	}

	val useBeam = true
	val (hmmDirname, hmmName) = {
//		("selected-hmms/", "Freq-Corpus-Ratio=100-Order=2-Unknown-Word-Threshold=17")
		("temp/", "Perceptron-Full-Averaging=Complete-Corpus-Ratio=100-Features=Base-Iterations=10-Order=2")
	}

	val hmmFilename = hmmDirname + hmmName + ".json"
	val conf = Configuration()
		.set(Configuration.DECODER, if (useBeam) BeamDecoder else FullDecoder)
		.set(BeamDecoder.BEAM, 3)
		.completeForDecoding

	val (hmm, loadTime) = timed(s"Loading '$hmmFilename'") {
		fromFile(new File(hmmFilename))
	}

	for (i <- 1 to 50) {
		decode(hmm, conf, hmmName, testCorpus)
	}

	def decode(hmm: HiddenMarkovModel,
	           conf: Configuration, hmmName: String,
	           refCorpus: Corpus[Sequence with Annotation]) {

		val decoder = conf(Configuration.DECODER).instantiate(hmm, conf)
		val hypCorpus = decoder.decode(refCorpus)

		val results = Checking.check(conf, hmm, refCorpus, hypCorpus, Lexica.CATEGORIES,
			new File("temp/" + conf.toFilename + "-on-" + hmmName + ".check"))

		results.display()
	}
}

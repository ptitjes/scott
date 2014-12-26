package io.github.ptitjes.scott.nl.lang.fr

import java.io.File

import io.github.ptitjes.scott.HiddenMarkovModel._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott.analysis.Checking
import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.decoders._

object loadFTBAndCheck extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS, Lexica.CATEGORIES)
	val testCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS, Lexica.CATEGORIES)

	val (hmmDirname, hmmName) = {
		//		("selected-hmms/", "Freq-Corpus-Ratio=100-Order=2-Unknown-Word-Threshold=17")
		("temp/", "Perceptron-Full-Averaging=Complete-Corpus-Ratio=100-Features=Base-Iterations=10-Order=2")
	}

	val hmmFilename = "temp/FTB-Coarse-10.hmm"

	val (hmm, loadTime) = timed(s"Loading '$hmmFilename'") {
		readFrom[NLToken, NLToken with NLPosTag](new File(hmmFilename))
	}

	val decoder = new BeamDecoder(hmm, 5)

	for (i <- 1 to 50) {
		val hypCorpus = decoder.decode(testCorpus)

		val results = Checking.check(hmm, testCorpus, hypCorpus, Lexica.CATEGORIES,
			new File("temp/Decode-on-" + hmmName + ".check"))

		results.display()
	}
}

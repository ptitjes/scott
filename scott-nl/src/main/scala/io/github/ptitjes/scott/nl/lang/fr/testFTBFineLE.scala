package io.github.ptitjes.scott.nl.lang.fr

import java.io.File

import io.github.ptitjes.scott.api.HiddenMarkovModel._
import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.decoders.BeamDecoder
import io.github.ptitjes.scott.nl.analysis.Checking
import io.github.ptitjes.scott.nl.conll.CoNLLToken
import io.github.ptitjes.scott.nl.corpora.Corpora._
import io.github.ptitjes.scott.utils.Utils._

object testFTBFineLE extends App {

	val ftbPath = args(0)

	val (trainCorpus, devCorpus, testCorpus) = FTB.parseSplitFine(ftbPath)

	val hmmName = "FTB-Fine-LE-" + 10
	val hmmFile = new File("temp/" + hmmName + ".hmm")

	val (hmm, _) = timed("Loading model") {
		readFrom[NLToken, NLToken with NLPosTag](hmmFile)
	}

	val decoder = new BeamDecoder(hmm, 5)

	for (i <- 0 until 10) {
		decode(devCorpus)
		decode(testCorpus)
	}

	println()

	def decode(corpus: DataSet[CoNLLToken]) {
		Checking.check(hmm, corpus, decoder.decode(corpus)).display()
	}
}

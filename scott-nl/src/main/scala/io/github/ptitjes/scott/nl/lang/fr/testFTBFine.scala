package io.github.ptitjes.scott.nl.lang.fr

import java.io.File

import io.github.ptitjes.scott.api.HiddenMarkovModel._
import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.decoders.BeamDecoder
import io.github.ptitjes.scott.nl.analysis.Checking
import io.github.ptitjes.scott.nl.conll.CoNLLToken
import io.github.ptitjes.scott.nl.corpora.Corpora._
import io.github.ptitjes.scott.utils.Utils._

object testFTBFine extends App {

	val ftbPath = args(0)

	val (trainCorpus, devCorpus, testCorpus) = FTB.parseSplitFine(ftbPath)

	val hmmName = "FTB-Fine-" + 10
	val hmmFile = new File("temp/" + hmmName + ".hmm")

	val (loadedHmm, _) = timed("Loading model") {
		readFrom[NLToken, NLToken with NLPosTag](hmmFile)
	}

	decode(loadedHmm, "Loaded-" + hmmName, devCorpus)
	decode(loadedHmm, "Loaded-" + hmmName, testCorpus)

	println()

	def decode(hmm: HiddenMarkovModel[NLToken, NLToken with NLPosTag], hmmName: String, corpus: DataSet[CoNLLToken]) {
		val decoder = new BeamDecoder(hmm)
		val hypCorpus = decoder.decode(devCorpus)
		Checking.check(hmm, devCorpus, hypCorpus, devCorpus.tagSet, new File("temp/Decode-on-" + hmmName + ".check")).display()
	}
}

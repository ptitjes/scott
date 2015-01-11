package io.github.ptitjes.scott.nl.lang.fr

import io.github.ptitjes.scott.nl.conll.CoNLLXParser
import io.github.ptitjes.scott.nl.corpora.Lexica
import io.github.ptitjes.scott.nl.lang.fr.testFTBCoarse._

import scala.io.Source

/**
 * @author Didier Villevalois
 */
object testDistributions extends App {

	val ftbPath = args(0)

	val corpus = FTB.parseFullFine(ftbPath)

	val word = "en"

	for (
		(s, index) <- corpus.sequences.zipWithIndex;
		t <- s.tokens
	) {
		if (t.word.string == word) {
			println("\t\t" + index + "\t" + t.coarseTag)
		}
	}
}

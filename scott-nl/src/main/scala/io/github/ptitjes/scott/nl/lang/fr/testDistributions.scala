package io.github.ptitjes.scott.nl.lang.fr

import io.github.ptitjes.scott.corpora.Lexica
import io.github.ptitjes.scott.nl.conll.CoNLLXParser

import scala.io.Source

/**
 * @author Didier Villevalois
 */
object testDistributions extends App {

	val PATH_TO_FTB = "/home/didier/Documents/Work/Master/DM/InfStat/ftb"
	val CONLL_CORPUS = "ftb4+mc+undocpd+fct+structmod110908"

	val fullCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + ".dep_conll"

	val parser = new CoNLLXParser
	val profile = FTB.CoNLLProfile
	val tagSet = profile.coarseTagSet
	val corpus = parser.parse(profile, Source.fromFile(fullCorpusPath), Lexica.WORDS)

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

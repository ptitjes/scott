package io.github.ptitjes.scott.nl.lang.fr

import io.github.ptitjes.scott.corpora.TagSet
import io.github.ptitjes.scott.nl.conll.CoNLLXParser

/**
 * @author Didier Villevalois
 */
object FTB {

	object CoNLLProfile extends CoNLLXParser.Profile(CoarsePosTags, PosTags)

	object CoarsePosTags extends TagSet(
		IndexedSeq(
			"A",
			"ADV",
			"C",
			"CL",
			"D",
			"ET",
			"I",
			"N",
			"P",
			"P+D",
			"P+PRO",
			"PONCT",
			"PREF",
			"PRO",
			"V"
		)
	)

	object PosTags extends TagSet(
		IndexedSeq(
			"ADJ",
			"ADJWH",
			"ADV",
			"ADVWH",
			"CC",
			"CLO",
			"CLR",
			"CLS",
			"CS",
			"DET",
			"DETWH",
			"ET",
			"I",
			"NC",
			"NPP",
			"P",
			"P+D",
			"P+PRO",
			"PONCT",
			"PREF",
			"PRO",
			"PROREL",
			"PROWH",
			"V",
			"VIMP",
			"VINF",
			"VPP",
			"VPR",
			"VS"
		)
	)

}

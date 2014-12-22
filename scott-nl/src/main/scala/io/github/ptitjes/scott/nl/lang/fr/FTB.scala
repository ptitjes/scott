package io.github.ptitjes.scott.nl.lang.fr

import io.github.ptitjes.scott.corpora.TagSet
import io.github.ptitjes.scott.nl.conll.ConllParser

/**
 * @author Didier Villevalois
 */
object FTB {

	object BasicConllProfile extends ConllParser.Profile(1, 2, 3, BasicTags)

	object ExtendedConllProfile extends ConllParser.Profile(1, 2, 4, ExtendedTags)

	object BasicTags extends TagSet(
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

	object ExtendedTags extends TagSet(
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

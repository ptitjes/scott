package io.github.ptitjes.scott.nl.lang.fr

import io.github.ptitjes.scott.api.TagSet
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

	def tagToCoarseTag(tag:Int) = PosTags(tag) match {
		case "ADJ" => CoarsePosTags("A")
		case "ADJWH" => CoarsePosTags("A")
		case "ADV" => CoarsePosTags("ADV")
		case "ADVWH" => CoarsePosTags("ADV")
		case "CC" => CoarsePosTags("C")
		case "CLO" => CoarsePosTags("CL")
		case "CLR" => CoarsePosTags("CL")
		case "CLS" => CoarsePosTags("CL")
		case "CS" => CoarsePosTags("C")
		case "DET" => CoarsePosTags("D")
		case "DETWH" => CoarsePosTags("D")
		case "ET" => CoarsePosTags("ET")
		case "I" => CoarsePosTags("I")
		case "NC" => CoarsePosTags("N")
		case "NPP" => CoarsePosTags("N")
		case "P" => CoarsePosTags("P")
		case "P+D" => CoarsePosTags("P+D")
		case "P+PRO" => CoarsePosTags("P+PRO")
		case "PONCT" => CoarsePosTags("PONCT")
		case "PREF" => CoarsePosTags("PREF")
		case "PRO" => CoarsePosTags("PRO")
		case "PROREL" => CoarsePosTags("PRO")
		case "PROWH" => CoarsePosTags("PRO")
		case "V" => CoarsePosTags("V")
		case "VIMP" => CoarsePosTags("V")
		case "VINF" => CoarsePosTags("V")
		case "VPP" => CoarsePosTags("V")
		case "VPR" => CoarsePosTags("V")
		case "VS" => CoarsePosTags("V")
	}
}

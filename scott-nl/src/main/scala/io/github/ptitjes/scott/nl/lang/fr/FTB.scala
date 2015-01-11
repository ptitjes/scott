package io.github.ptitjes.scott.nl.lang.fr

import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.nl.conll._
import io.github.ptitjes.scott.nl.corpora.Corpora.{NLPosTag, NLToken}
import io.github.ptitjes.scott.nl.corpora.Lexica

import scala.io.Source
import scala.util.Properties

/**
 * @author Didier Villevalois
 */
object FTB {

	val PATH_TO_FTB = "scott.nl.lang.fr.ftb"
	val CONLL_DIRECTORY = "/corpus-conll/"
	val CONLL_CORPUS = "ftb4+mc+undocpd+fct+structmod110908"

	def path: String =
		if (!Properties.propIsSet(PATH_TO_FTB)) throw new IllegalStateException("Missing property " + PATH_TO_FTB)
		else Properties.propOrNull(PATH_TO_FTB)

	def parseSplitCoarse(): (DataSet[CoNLLCoarseToken], DataSet[CoNLLCoarseToken], DataSet[CoNLLCoarseToken]) = {
		val conllPath = path + CONLL_DIRECTORY
		val parser = new CoNLLXParser
		(parser.parseCoarse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + "_1.dep_conll"), Lexica.WORDS),
			parser.parseCoarse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + "_2.dep_conll"), Lexica.WORDS),
			parser.parseCoarse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + "_3.dep_conll"), Lexica.WORDS))
	}

	def parseSplitFine(): (DataSet[CoNLLToken], DataSet[CoNLLToken], DataSet[CoNLLToken]) = {
		val conllPath = path + CONLL_DIRECTORY
		val parser = new CoNLLXParser
		(parser.parse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + "_1.dep_conll"), Lexica.WORDS),
			parser.parse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + "_2.dep_conll"), Lexica.WORDS),
			parser.parse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + "_3.dep_conll"), Lexica.WORDS))
	}

	def parseFullCoarse(): DataSet[CoNLLCoarseToken] = {
		val conllPath = path + CONLL_DIRECTORY
		val parser = new CoNLLXParser
		parser.parseCoarse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + ".dep_conll"), Lexica.WORDS)
	}

	def parseFullFine(): DataSet[CoNLLToken] = {
		val conllPath = path + CONLL_DIRECTORY
		val parser = new CoNLLXParser
		parser.parse(CoNLLProfile, Source.fromFile(conllPath + CONLL_CORPUS + ".dep_conll"), Lexica.WORDS)
	}

	val wordCodeExtractor: NLToken => Int = _.word.code
	val tagExtractor: NLToken with NLPosTag => Int = _.tag

	val coarseTokenBuilder: (NLToken, Int) => CoNLLCoarseToken =
		(token, tag) => CoNLLCoarseToken(token.word, tag)

	val fineTokenBuilder: (NLToken, Int) => CoNLLToken =
		(token, tag) => CoNLLToken(token.word, FTB.tagToCoarseTag(tag), tag)

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

	def tagToCoarseTag(tag: Int) = PosTags(tag) match {
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

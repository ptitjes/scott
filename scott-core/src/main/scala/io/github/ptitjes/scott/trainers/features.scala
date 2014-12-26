package io.github.ptitjes.scott.trainers

import io.github.ptitjes.scott.Features._
import io.github.ptitjes.scott.corpora._

/**
 * @author Didier Villevalois
 */
object features {

	import scala.language.postfixOps

	object BaseFeatures extends NLFeatureSetTemplate {

		def name = "Base"

		def features(order: Int) =
			List(
				// Lexical features
				FeatureTemplate(w(0) code),
				FeatureTemplate(w(0) contains '-'),
				FeatureTemplate(w(0) containsNumber),
				FeatureTemplate(w(0) containsOnlyNumber),
				FeatureTemplate(w(0) containsOnlyNumber, t(-1)),
				FeatureTemplate(w(0) containsOnlyNumber, w(1) equalTo "%"),
				FeatureTemplate(w(0) containsUppercase),
				FeatureTemplate(w(0) containsOnlyUppercase),
				FeatureTemplate(w(0) containsUppercase, not(t(-1) equalTo -1), t(-1))) ++
				(for (l <- 0 until 4) yield FeatureTemplate(for (i <- 0 to l) yield w(0).prefix(i))) ++
				(for (l <- 0 until 4) yield FeatureTemplate(for (i <- 0 to l) yield w(0).suffix(i))) ++
				// Contextual features
				(for (o <- 1 to order) yield FeatureTemplate(for (i <- 1 to o) yield t(-i))) ++
				(for (i <- 1 to order) yield FeatureTemplate(w(-i) code)) ++
				(for (i <- 1 to order) yield FeatureTemplate(w(i) code))
	}

}

trait NLFeatureSetTemplate extends FeatureSetTemplate[NLToken, NLToken with NLPosTag] {

	def w(i: Int) = token(i).word

	implicit class RichNLTokenExtractor(e: Extractor[NLToken, Option[NLToken]]) {

		def word = ENLTokenWord(e)
	}

	implicit class RichNLWordExtractor[X](e: Extractor[X, Option[Word]]) {

		def code = EWordCode(e)

		def string = EWordString(e)

		def prefix(i: Int) = EPrefixChar(string, i)

		def suffix(i: Int) = ESuffixChar(string, i)

		def contains(char: Char) = PContains(string, char)

		def containsUppercase = PContainsUppercase(string)

		def containsOnlyUppercase = PUppercaseOnly(string)

		def containsNumber = PContainsNumber(string)

		def containsOnlyNumber = PNumberOnly(string)

		def equalTo(value: String) = PEqualTo(string, value)
	}

}
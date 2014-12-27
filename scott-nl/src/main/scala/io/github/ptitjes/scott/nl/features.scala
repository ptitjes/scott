package io.github.ptitjes.scott.nl

import io.github.ptitjes.scott.api.Features._
import io.github.ptitjes.scott.nl.corpora.Corpora._
import io.github.ptitjes.scott.nl.corpora.Word
import io.github.ptitjes.scott.trainers._

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
				FeatureTemplate(word(0) code),
				FeatureTemplate(word(0) contains '-'),
				FeatureTemplate(word(0) containsNumber),
				FeatureTemplate(word(0) containsOnlyNumber),
				FeatureTemplate(word(0) containsOnlyNumber, tag(-1)),
				FeatureTemplate(word(0) containsOnlyNumber, word(-1) code),
				FeatureTemplate(word(0) containsUppercase),
				FeatureTemplate(word(0) containsOnlyUppercase),
				FeatureTemplate(word(0) containsUppercase, not(tag(-1) equalTo -1), tag(-1))) ++
				(for (l <- 0 until 4) yield FeatureTemplate(for (i <- 0 to l) yield word(0).prefix(i))) ++
				(for (l <- 0 until 4) yield FeatureTemplate(for (i <- 0 to l) yield word(0).suffix(i))) ++
				// Contextual features
				(for (o <- 1 to order) yield FeatureTemplate(for (i <- 1 to o) yield tag(-i))) ++
				(for (i <- 1 to order) yield FeatureTemplate(word(-i) code)) ++
				(for (i <- 1 to order) yield FeatureTemplate(word(i) code))
	}

}

trait NLFeatureSetTemplate extends FeatureSetTemplate[NLToken, NLToken with NLPosTag] {

	def word(i: Int) = token(i).word

	implicit class RichNLTokenExtractor(e: Selector[NLToken, NLToken]) {

		def word = e.select(_.word)
	}

	implicit class RichNLWordExtractor[X](e: Selector[X, Word]) {

		def code = e.select(_.code)

		def string = e.select(_.string)

		def prefix(i: Int) = string.prefix(i)

		def suffix(i: Int) = string.suffix(i)

		def contains(char: Char) = string.contains(char)

		def containsUppercase = string.containsUppercase

		def containsOnlyUppercase = string.containsOnlyUppercase

		def containsNumber = string.containsNumber

		def containsOnlyNumber = string.containsOnlyNumber

		def equalTo(value: String) = string.equalTo(value)
	}

	implicit class RichStringExtractor[X](e: Selector[X, String]) {

		def prefix(i: Int) = e.optionSelect(s => if (s.length > i) Some(s.charAt(i)) else None)

		def suffix(i: Int) = e.optionSelect(s => if (s.length > i) Some(s.charAt(s.length - i - 1)) else None)

		def contains(char: Char) = e.test(_.indexOf(char) != -1)

		def containsUppercase = e.test(_.exists(_.isUpper))

		def containsOnlyUppercase = e.test(_.forall(_.isUpper))

		def containsNumber = e.test(_.exists(_.isDigit))

		def containsOnlyNumber = e.test(_.forall(c => c.isDigit || c == '.' || c == ','))

		def equalTo(value: String) = e.test(_ == value)
	}

}
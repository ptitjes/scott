package io.github.ptitjes.scott.nl

import io.github.ptitjes.scott.api.Features._
import io.github.ptitjes.scott.nl.corpora.Corpora._
import io.github.ptitjes.scott.nl.corpora.Word
import io.github.ptitjes.scott.trainers._

/**
 * @author Didier Villevalois
 */
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

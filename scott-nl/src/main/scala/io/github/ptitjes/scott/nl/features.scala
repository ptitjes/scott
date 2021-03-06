package io.github.ptitjes.scott.nl

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


package io.github.ptitjes.scott.nl

import io.github.ptitjes.scott.trainers._
import io.github.ptitjes.scott.utils.Trie

import scala.collection.Set
import scala.collection.BitSet

/**
 * @author Didier Villevalois
 */
object lefffEnhancedFeatures {

	import scala.language.postfixOps

	class Features(lefff: Trie[BitSet]) extends NLFeatureSetTemplate with Serializable {

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
				(for (i <- 1 to order) yield FeatureTemplate(word(i) code)) ++
				// Lefff-based Lexical features
				List(
					FeatureTemplate(wordLefffTags(0))
				) ++
				// Lefff-based Contextual features
				(for (i <- 1 to order) yield FeatureTemplate(wordLefffTags(-i))) ++
				(for (i <- 1 to order) yield FeatureTemplate(wordLefffTags(i)))

		def wordLefffTags(i: Int) = word(i).string.select(lefff(_).map(_.toBitMask(0)))
	}

}

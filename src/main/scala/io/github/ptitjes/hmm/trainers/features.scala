package io.github.ptitjes.hmm.trainers

/**
 * @author Didier Villevalois
 */
object features {

	import scala.language.postfixOps

	object BaseFeatures extends FeatureSetTemplate {

		def name = "Base"

		def features(order: Int) =
			List(
				// Lexical features
				FeatureTemplate(w(0)),
				FeatureTemplate(w(0) contains '-'),
				FeatureTemplate(w(0) containsNumber),
				FeatureTemplate(w(0) containsUppercase),
				FeatureTemplate(w(0) containsOnlyUppercase),
				FeatureTemplate(w(0) containsUppercase, not(t(-1) === -1), t(-1))) ++
				(for (l <- 0 until 4) yield FeatureTemplate(for (i <- 0 to l) yield p(i))) ++
				(for (l <- 0 until 4) yield FeatureTemplate(for (i <- 0 to l) yield s(i))) ++
				// Contextual features
				(for (o <- 1 to order) yield FeatureTemplate(for (i <- 1 to o) yield t(-i))) ++
				(for (i <- 1 to order) yield FeatureTemplate(w(-i))) ++
				(for (i <- 1 to order) yield FeatureTemplate(w(i)))
	}
}

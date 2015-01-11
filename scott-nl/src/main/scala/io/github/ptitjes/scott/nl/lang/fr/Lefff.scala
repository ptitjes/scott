package io.github.ptitjes.scott.nl.lang.fr

import scala.util.Properties

/**
 * @author Didier Villevalois
 */
object Lefff {

	val PATH_TO_LEFFF = "scott.nl.lang.fr.lefff"

	def path: String =
		if (!Properties.propIsSet(PATH_TO_LEFFF)) throw new IllegalStateException("Missing property " + PATH_TO_LEFFF)
		else Properties.propOrNull(PATH_TO_LEFFF)


}

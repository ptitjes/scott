package io.github.ptitjes.scott.nl.corpora

/**
 * @author Didier Villevalois
 */
object Corpora {

	trait NLToken {
		def word: Word
	}

	trait NLCoarsePosTag {
		def coarseTag: Int
	}

	trait NLPosTag {
		def tag: Int
	}
}

package io.github.ptitjes.scott.api

import io.github.ptitjes.scott.utils.Trie

/**
 * @author Didier Villevalois
 */
case class TagSet(tags: IndexedSeq[String]) {

	private val stringToCode = Trie[Int]() ++ tags.zipWithIndex
	private val maxLength = tags.map(_.length).reduce(math.max)

	def size: Int = tags.size

	def apply(i: Int): String = tags(i)

	def apply(s: String): Int = stringToCode(s).get

	def padded(i: Int) = {
		val tag = tags(i)
		tag + " " * (maxLength - tag.length)
	}
}

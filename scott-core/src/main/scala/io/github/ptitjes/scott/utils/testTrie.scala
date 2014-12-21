package io.github.ptitjes.scott.utils

/**
 * @author Didier Villevalois
 */
object testTrie extends App {

	var t = Trie[String]()

	t = t + ("ab" -> "ab")
	println(t)
	t = t + ("abc" -> "abc")
	println(t)
}

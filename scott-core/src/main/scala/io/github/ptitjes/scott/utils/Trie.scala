package io.github.ptitjes.scott.utils

import scala.collection.SortedMap

object Trie {

	def apply[A](values: (String, A)*) = empty ++ values

	def empty[A]: Trie[A] = new Trie[A]()
}

/**
 * An immutable single-valued String based index that allows lookups by prefix
 */
case class Trie[A](normalise: String => String = (s: String) => s, root: TrieNode[A] = TrieNode[A]()) {

	def apply(key: String): Option[A] = root.nodeFor(asChars(key)).flatMap(_.value)

	/**
	 * Get all the values that match the given prefix.
	 */
	def allWithPrefix(prefix: String): List[A] =
		root.nodeFor(asChars(prefix)).toList.flatMap(_.all)

	/**
	 * Add the given key value pair to the index.
	 */
	def +(keyValue: (String, A)): Trie[A] =
		Trie(normalise, root.put(asChars(keyValue._1), keyValue._2))

	/**
	 * Add the given key value pairs to the index
	 */
	def ++(keyValues: TraversableOnce[(String, A)]): Trie[A] = keyValues.foldLeft(this)(_ + _)

	/**
	 * Index the given value with the given keys
	 */
	def index(keys: TraversableOnce[String], value: A) = this ++ keys.map(_ -> value)

	/**
	 * Remove the given value at the given key.
	 */
	def -(keyValue: (String, A)): Trie[A] =
		Trie(normalise, root.remove(asChars(keyValue._1), _ == keyValue._2))

	/**
	 * Filter the values at the given key with the given predicate
	 */
	def deindex(keys: TraversableOnce[String], predicate: A => Boolean): Trie[A] =
		keys.foldLeft(this)((trie, key) => Trie(normalise, root.remove(asChars(key), predicate)))

	/**
	 * Whether this index is empty.
	 */
	def isEmpty = root.isEmpty

	private def asChars(key: String): List[Char] = normalise(key).toList
}

case class TrieNode[A](nodes: SortedMap[Char, TrieNode[A]] = SortedMap[Char, TrieNode[A]](),
                       value: Option[A] = None) {

	def isEmpty = value.isEmpty && nodes.isEmpty

	def remove(key: List[Char], predicate: A => Boolean): TrieNode[A] = key match {
		case Nil => TrieNode(nodes, value.filterNot(predicate))
		case c :: rest =>
			nodes.get(c).map(_.remove(rest, predicate)).filterNot(_.isEmpty) match {
				case Some(node) => new TrieNode(nodes + (c -> node), value)
				case None => TrieNode(nodes - c, value)
			}
	}

	def put(key: List[Char], newValue: A): TrieNode[A] = key match {
		case Nil => TrieNode(nodes, Some(newValue))
		case c :: rest =>
			val node = nodes.getOrElse(c, TrieNode()).put(rest, newValue)
			new TrieNode(nodes + (c -> node), value)
	}

	def all: Set[A] = nodes.values.toSet.flatMap((trie: TrieNode[A]) => trie.all) ++ value

	def nodeFor(prefix: List[Char]): Option[TrieNode[A]] = prefix match {
		case Nil => Some(this)
		case c :: rest => for {
			child <- nodes.get(c)
			node <- child.nodeFor(rest)
		} yield node
	}
}
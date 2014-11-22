package io.github.ptitjes.hmm

import scala.collection._

object Features {

	import Corpora._

	sealed trait Predicate extends (History => Boolean) {

		def apply(h: History): Boolean = this match {
			case PContainsUppercase() => h.word.string.exists(_.isUpper)
			case PUppercaseOnly() => h.word.string.forall(_.isUpper)
			case PContainsNumber() => h.word.string.exists(_.isDigit)
			case PContains(v) => h.word.string.indexOf(v) != -1
			case PLength(i, l) => h.wordAt(i).string.length == l
		}
	}

	case class PContainsUppercase() extends Predicate

	case class PUppercaseOnly() extends Predicate

	case class PContainsNumber() extends Predicate

	case class PContains(value: Char) extends Predicate

	case class PLength(index: Int, l: Int) extends Predicate

	sealed trait Extractor[T] extends (History => T)

	case class EPrefixChar(index: Int) extends Extractor[Char] {

		def apply(h: History): Char = {
			val word = h.word.string
			if (word.length > index) word.charAt(index) else 0.toChar
		}
	}

	case class ESuffixChar(index: Int) extends Extractor[Char] {

		def apply(h: History): Char = {
			val word = h.word.string
			if (word.length > index) word.charAt(word.length - index - 1) else 0.toChar
		}
	}

	case class EWordCode(index: Int) extends Extractor[Int] {

		def apply(h: History): Int = {
			val word = h.wordAt(index)
			if (word == null) -1 else word.code
		}
	}

	case class EPreviousTag(index: Int) extends Extractor[Int] {

		def apply(h: History): Int = h.previousTag(index)
	}

	case class Weights(breadth: Int, tags: Set[Int], values: Array[Double]) {

		def this(breadth: Int, tags: Set[Int]) =
			this(breadth, tags, Array.ofDim(breadth))

		def apply(key: Int) = values(key)

		def update(key: Int, value: Double) =
			if (!tags(key)) throw new ArrayIndexOutOfBoundsException(key)
			else values(key) = value

		def foreach[U](f: (Int, Double) => U): Unit =
			tags.foreach(t => f(t, values(t)))

		def map(f: Double => Double): Weights =
			new Weights(breadth, tags, values.map(f))
	}

	sealed trait FeatureTree[T] {

		def foreachMatching(h: History, tags: BitSet)(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreachMatching(h, tags)(f))
			case FTDispatchChar(extract, children, filter) =>
				if ((tags & filter).nonEmpty) {
					val key = extract(h)
					if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
				}
			case FTDispatchInt(extract, children) =>
				val key = extract(h)
				if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
			case FTGuard(predicate, child) => if (predicate(h)) child.foreachMatching(h, tags)(f)
			case FTLeaf(weights, filter) => if ((tags & filter).nonEmpty) f(weights)
		}

		def foreach(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(f))
			case FTDispatchChar(extract, children, tags) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTDispatchInt(extract, children) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTGuard(predicate, child) => child.foreach(f)
			case FTLeaf(weights, filter) => f(weights)
		}

		def map[U](f: T => U): FeatureTree[U] = this match {
			case FTConjunction(children) => FTConjunction(children.map(c => c.map(f)))
			case FTDispatchChar(extract, children, tags) =>
				FTDispatchChar(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTDispatchInt(extract, children) =>
				FTDispatchInt(extract, children.map { case (k, c) => (k, c.map(f))})
			case FTGuard(predicate, child) => FTGuard(predicate, child.map(f))
			case FTLeaf(weights, filter) => FTLeaf(f(weights), filter)
		}
	}

	case class FTConjunction[T](children: Seq[FeatureTree[T]]) extends FeatureTree[T]

	case class FTDispatchChar[T](extract: Extractor[Char],
	                             children: Map[Char, FeatureTree[T]],
	                             tags: BitSet) extends FeatureTree[T]

	case class FTDispatchInt[T](extract: Extractor[Int], children: Map[Int, FeatureTree[T]]) extends FeatureTree[T]

	case class FTGuard[T](predicate: Predicate, child: FeatureTree[T]) extends FeatureTree[T]

	case class FTLeaf[T](weights: T, tags: BitSet) extends FeatureTree[T]

	def makeNgramTree[T](ngrams: Set[(List[Int], Int)], f: Set[Int] => T): FeatureTree[T] = {

		def aux(ngrams: Map[List[Int], Set[Int]], index: Int): FeatureTree[T] = {
			if (ngrams.size == 1) {
				val allTags = ngrams.foldLeft(Set[Int]()) { case (collected, (_, tags)) => collected ++ tags}
				FTLeaf(f(allTags), BitSet() ++ allTags)
			} else {
				val perPrevious = ngrams.groupBy(_._1.head).mapValues(
					_.map { case (seq, tags) => (seq.tail, tags)}
				)
				FTDispatchInt(
					EPreviousTag(index),
					perPrevious.map {
						case (tag, innerNgrams) => tag -> aux(innerNgrams, index + 1)
					}
				)
			}
		}

		val ngramTags = ngrams.groupBy(_._1).mapValues(_.map(_._2))
		aux(ngramTags, 1)
	}

	def makeWordTree[T](index: Int, words: Map[Int, Set[Int]], f: Set[Int] => T): FeatureTree[T] =
		FTDispatchInt(EWordCode(index),
			words.foldLeft(Map[Int, FeatureTree[T]]()) {
				case (m, (w, tags)) => m + (w -> FTLeaf(f(tags), BitSet() ++ tags))
			})

	def makePrefixTree[T](words: Map[String, Set[Int]], f: Set[Int] => T): FeatureTree[T] =
		makeCharTree(words, 4,
			s => (s.charAt(0), s.substring(1)),
			i => EPrefixChar(i),
			tags => FTLeaf(f(tags), BitSet() ++ tags)
		)

	def makeSuffixTree[T](words: Map[String, Set[Int]], f: Set[Int] => T): FeatureTree[T] =
		makeCharTree(words, 4,
			s => (s.last, s.substring(0, s.length - 1)),
			i => ESuffixChar(i),
			tags => FTLeaf(f(tags), BitSet() ++ tags)
		)

	def makeCharTree[T](words: Map[String, Set[Int]], maxLength: Int,
	                    cruncher: String => (Char, String),
	                    extractor: Int => Extractor[Char],
	                    leaf: Set[Int] => FeatureTree[T]): FeatureTree[T] = {

		val allTags = (0 until 15).toSet

		def aux(tree: Tree, index: Int): FeatureTree[T] = tree match {
			case Node(children, filter) =>
				val childrenFeatures = children.map {
					case (char, child) =>
						(char, aux(child, index + 1))
				}
				FTConjunction(
					leaf(filter) ::
						FTDispatchChar(extractor(index), childrenFeatures, BitSet() ++ filter) ::
						Nil
				)
			case Leaf(children, filter) => leaf(filter)
		}

		aux(makeTree(words, cruncher, maxLength), 0)
	}

	trait Tree

	case class Node(children: Map[Char, Tree], filter: Set[Int]) extends Tree

	case class Leaf(children: Map[String, Set[Int]], filter: Set[Int]) extends Tree

	def makeTree(words: Map[String, Set[Int]], cruncher: String => (Char, String), max: Int): Tree = {

		def crunch(l: Map[String, Set[Int]]): Map[Char, Map[String, Set[Int]]] = {
			l.toList.map {
				case (word, tags) =>
					val (head, rest) = cruncher(word)
					(head, (rest, tags))
			}.groupBy(_._1).mapValues(
			    _.map(_._2).groupBy(_._1).mapValues(
				    _.map(_._2).fold(Set[Int]())(_ ++ _)
			    )
				)
		}

		def doMakeTree(words: Map[String, Set[Int]], index: Int): (Tree, Set[Int]) = {
			if (index == max) {
				val allTags = words.foldLeft(Set[Int]()) {
					case (collected, (str, tags)) => collected ++ tags
				}
				(Leaf(words, allTags), allTags)
			} else {
				val crunched = crunch(words)
				val charToTreeAndTags = crunched.map {
					case (char, stringsToTags) =>
						val (tree, tags) = doMakeTree(stringsToTags - "", index + 1)

						val completeTags =
							if (stringsToTags.contains("")) tags ++ stringsToTags("")
							else tags

						(char, (tree, completeTags))
				}

				val allTags = charToTreeAndTags.foldLeft(Set[Int]()) {
					case (collected, (_, (_, tags))) => collected ++ tags
				}
				val charToTree = charToTreeAndTags.map {
					case (char, (tree, _)) => (char, tree)
				}
				(Node(charToTree, allTags), allTags)
			}
		}

		doMakeTree(words, 0)._1
	}
}

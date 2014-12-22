package io.github.ptitjes.scott

import io.github.ptitjes.scott.corpora.Corpora

import scala.collection._

object Features {

	import Corpora._

	sealed trait Extractor[T] extends (History => T)

	sealed trait Predicate extends Extractor[Boolean]

	sealed trait WordPredicate extends Predicate {

		def from: Extractor[String]

		def apply(h: History): Boolean = {
			val word = from(h)

			if (word == null) false
			else this match {
				case PContainsUppercase(_) => word.exists(_.isUpper)
				case PUppercaseOnly(_) => word.forall(_.isUpper)
				case PContainsNumber(_) => word.exists(_.isDigit)
				case PContains(_, v) => word.indexOf(v) != -1
			}
		}
	}

	case class PContainsUppercase(from: Extractor[String]) extends WordPredicate

	case class PUppercaseOnly(from: Extractor[String]) extends WordPredicate

	case class PContainsNumber(from: Extractor[String]) extends WordPredicate

	case class PContains(from: Extractor[String], value: Char) extends WordPredicate

	case class PTagEqual(from: Extractor[Int], value: Int) extends Predicate {

		def apply(h: History): Boolean = from(h) == value
	}

	case class PNot(from: Extractor[Boolean]) extends Predicate {

		def apply(h: History): Boolean = !from(h)
	}

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

	case class EWordString(index: Int) extends Extractor[String] {

		def apply(h: History): String = {
			val word = h.wordAt(index)
			if (word != null) word.string else null
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

	case class Weights(tags: BitSet, values: Array[Double]) {
		val tagsAsArray = tags.toArray

		def apply(key: Int) = values(key)

		def update(key: Int, value: Double) =
			if (!tags(key)) throw new ArrayIndexOutOfBoundsException(key)
			else values(key) = value

		def foreach[U](f: (Int, Double) => U): Unit =
			tagsAsArray.foreach(t => f(t, values(t)))

		def map(f: Double => Double): Weights =
			new Weights(tags, values.map(f))
	}

	sealed trait FeatureTree[T] {

		def foreachMatching(h: History, tags: BitSet)(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreachMatching(h, tags)(f))
			case FTDispatchInt(extract, children, filter) =>
				if (tags exists filter) {
					val key = extract(h)
					if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
				}
			case FTDispatchChar(extract, children, filter) =>
				if (tags exists filter) {
					val key = extract(h)
					if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
				}
			case FTGuard(predicate, child) => if (predicate(h)) child.foreachMatching(h, tags)(f)
			case FTLeaf(weights, filter) => if (tags exists filter) f(weights)
		}

		def foreach(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(f))
			case FTDispatchInt(extract, children, tags) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTDispatchChar(extract, children, tags) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTGuard(predicate, child) => child.foreach(f)
			case FTLeaf(weights, filter) => f(weights)
		}

		def map[U](f: T => U): FeatureTree[U] = this match {
			case FTConjunction(children) => FTConjunction(children.map(c => c.map(f)))
			case FTDispatchInt(extract, children, tags) =>
				FTDispatchInt(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTDispatchChar(extract, children, tags) =>
				FTDispatchChar(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTGuard(predicate, child) => FTGuard(predicate, child.map(f))
			case FTLeaf(weights, filter) => FTLeaf(f(weights), filter)
		}
	}

	case class FTConjunction[T](children: Seq[FeatureTree[T]]) extends FeatureTree[T]

	case class FTDispatchInt[T](extract: Extractor[Int],
	                            children: Map[Int, FeatureTree[T]],
	                            tags: BitSet) extends FeatureTree[T]

	case class FTDispatchChar[T](extract: Extractor[Char],
	                             children: Map[Char, FeatureTree[T]],
	                             tags: BitSet) extends FeatureTree[T]

	case class FTGuard[T](predicate: Extractor[Boolean], child: FeatureTree[T]) extends FeatureTree[T]

	case class FTLeaf[T](weights: T, tags: BitSet) extends FeatureTree[T]

}

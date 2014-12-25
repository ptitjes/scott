package io.github.ptitjes.scott

import java.io.Serializable

import io.github.ptitjes.scott.corpora._

import scala.collection._

object Features {

	sealed trait Extractor[X, T] extends ((History[X], IndexedSeq[Int]) => T) with Serializable

	sealed trait Predicate[X] extends Extractor[X, Boolean]

	sealed trait WordPredicate[X] extends Predicate[X] {

		def from: Extractor[X, String]

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Boolean = {
			val word = from(h, previousTags)

			if (word == null) false
			else this match {
				case PContainsUppercase(_) => word.exists(_.isUpper)
				case PUppercaseOnly(_) => word.forall(_.isUpper)
				case PContainsNumber(_) => word.exists(_.isDigit)
				case PNumberOnly(_) => word.forall(c => c.isDigit || c == '.' || c == ',')
				case PContains(_, v) => word.indexOf(v) != -1
			}
		}
	}

	case class PContainsUppercase[X](from: Extractor[X, String]) extends WordPredicate[X]

	case class PUppercaseOnly[X](from: Extractor[X, String]) extends WordPredicate[X]

	case class PContainsNumber[X](from: Extractor[X, String]) extends WordPredicate[X]

	case class PNumberOnly[X](from: Extractor[X, String]) extends WordPredicate[X]

	case class PContains[X](from: Extractor[X, String], value: Char) extends WordPredicate[X]

	case class PTagEqual[X](from: Extractor[X, Int], value: Int) extends Predicate[X] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Boolean = from(h, previousTags) == value
	}

	case class PNot[X](from: Extractor[X, Boolean]) extends Predicate[X] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Boolean = !from(h, previousTags)
	}

	case class EPrefixChar[X](from: Extractor[X, String], index: Int) extends Extractor[X, Char] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Char = {
			val word = from(h, previousTags)
			if (word.length > index) word.charAt(index) else 0.toChar
		}
	}

	case class ESuffixChar[X](from: Extractor[X, String], index: Int) extends Extractor[X, Char] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Char = {
			val word = from(h, previousTags)
			if (word.length > index) word.charAt(word.length - index - 1) else 0.toChar
		}
	}

	case class EWordString[X](from: Extractor[X, Option[Word]]) extends Extractor[X, String] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): String =
			from(h, previousTags).map(_.string).orNull
	}

	case class EWordCode[X](from: Extractor[X, Option[Word]]) extends Extractor[X, Int] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Int =
			from(h, previousTags).map(_.code).getOrElse(-1)
	}

	case class ENLTokenWord[X <: NLToken](from: Extractor[X, Option[X]]) extends Extractor[X, Option[Word]] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Option[Word] = from(h, previousTags).map(_.word)
	}

	case class EToken[X](index: Int) extends Extractor[X, Option[X]] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Option[X] = h.at(index)
	}

	case class EPreviousTag[X](index: Int) extends Extractor[X, Int] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Int = previousTags(-index - 1)
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

	sealed trait FeatureTree[X, T] extends Serializable {

		def size: Int = this match {
			case FTConjunction(children) => children.map(_.size).reduce(_ + _)
			case FTDispatchInt(extract, children, filter) => children.values.map(_.size).reduce(_ + _)
			case FTDispatchChar(extract, children, filter) => children.values.map(_.size).reduce(_ + _)
			case FTGuard(predicate, child) => child.size
			case FTLeaf(weights, filter) => filter.size
		}

		def foreachMatching(h: History[X], previousTags: IndexedSeq[Int], tags: BitSet)(f: T => Unit): Unit = this match {
			case FTConjunction(children) =>
				children.foreach(c => c.foreachMatching(h, previousTags, tags)(f))
			case FTDispatchInt(extract, children, filter) =>
				if (tags exists filter) {
					val key = extract(h, previousTags)
					if (children.contains(key)) children(key).foreachMatching(h, previousTags, tags)(f)
				}
			case FTDispatchChar(extract, children, filter) =>
				if (tags exists filter) {
					val key = extract(h, previousTags)
					if (children.contains(key)) children(key).foreachMatching(h, previousTags, tags)(f)
				}
			case FTGuard(predicate, child) =>
				if (predicate(h, previousTags)) child.foreachMatching(h, previousTags, tags)(f)
			case FTLeaf(weights, filter) =>
				if (tags exists filter) f(weights)
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

		def map[U](f: T => U): FeatureTree[X, U] = this match {
			case FTConjunction(children) => FTConjunction(children.map(c => c.map(f)))
			case FTDispatchInt(extract, children, tags) =>
				FTDispatchInt(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTDispatchChar(extract, children, tags) =>
				FTDispatchChar(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTGuard(predicate, child) => FTGuard(predicate, child.map(f))
			case FTLeaf(weights, filter) => FTLeaf(f(weights), filter)
		}
	}

	case class FTConjunction[X, T](children: Seq[FeatureTree[X, T]]) extends FeatureTree[X, T]

	case class FTDispatchInt[X, T](extract: Extractor[X, Int],
	                               children: Map[Int, FeatureTree[X, T]],
	                               tags: BitSet) extends FeatureTree[X, T]

	case class FTDispatchChar[X, T](extract: Extractor[X, Char],
	                                children: Map[Char, FeatureTree[X, T]],
	                                tags: BitSet) extends FeatureTree[X, T]

	case class FTGuard[X, T](predicate: Extractor[X, Boolean], child: FeatureTree[X, T]) extends FeatureTree[X, T]

	case class FTLeaf[X, T](weights: T, tags: BitSet) extends FeatureTree[X, T]

}

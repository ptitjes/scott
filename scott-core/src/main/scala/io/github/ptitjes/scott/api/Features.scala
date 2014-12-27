package io.github.ptitjes.scott.api

import java.io.Serializable

import scala.collection._

object Features {

	sealed trait Extractor[X, T] extends ((History[X], IndexedSeq[Int]) => Option[T]) with Serializable

	sealed trait Selector[X, T] extends Extractor[X, T] {
		def makeDispatch[W](values: Map[_, FeatureTree[X, W]], filter: BitSet): FeatureTree[X, W] =
			FTDispatch(this, values.asInstanceOf[Map[T, FeatureTree[X, W]]], filter)
	}

	case class EToken[X](index: Int) extends Selector[X, X] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Option[X] = h.at(index)
	}

	case class EPreviousTag[X](index: Int) extends Selector[X, Int] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Option[Int] = Some(previousTags(-index - 1))
	}

	case class PFreeSelector[X, T, U](from: Selector[X, T], selector: T => Option[U]) extends Selector[X, U] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Option[U] = from(h, previousTags).flatMap(selector)
	}

	sealed trait Predicate[X] extends Extractor[X, Boolean] {
		def makeGuard[W](value: FeatureTree[X, W], filter: BitSet): FeatureTree[X, W] =
			FTGuard(this, value.asInstanceOf[FeatureTree[X, W]])
	}

	case class PNot[X](from: Predicate[X]) extends Predicate[X] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Option[Boolean] = from(h, previousTags).map(!_)
	}

	case class PFreePredicate[X, T](from: Selector[X, T], predicate: T => Boolean) extends Predicate[X] {

		def apply(h: History[X], previousTags: IndexedSeq[Int]): Option[Boolean] = from(h, previousTags).map(predicate)
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
			case FTDispatch(extract, children, filter) => children.values.map(_.size).reduce(_ + _)
			case FTGuard(predicate, child) => child.size
			case FTLeaf(weights, filter) => filter.size
		}

		def foreachMatching(h: History[X], previousTags: IndexedSeq[Int], tags: BitSet)(f: T => Unit): Unit = this match {
			case FTConjunction(children) =>
				children.foreach(c => c.foreachMatching(h, previousTags, tags)(f))
			case FTDispatch(extract, children, filter) =>
				if (tags exists filter) {
					val key = extract(h, previousTags)
					if (key.exists(children.contains)) children(key.get).foreachMatching(h, previousTags, tags)(f)
				}
			case FTGuard(predicate, child) =>
				if (predicate(h, previousTags).contains(true)) child.foreachMatching(h, previousTags, tags)(f)
			case FTLeaf(weights, filter) =>
				if (tags exists filter) f(weights)
		}

		def foreach(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(f))
			case FTDispatch(extract, children, tags) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTGuard(predicate, child) => child.foreach(f)
			case FTLeaf(weights, filter) => f(weights)
		}

		def map[U](f: T => U): FeatureTree[X, U] = this match {
			case FTConjunction(children) => FTConjunction(children.map(c => c.map(f)))
			case FTDispatch(extract, children, tags) =>
				FTDispatch(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTGuard(predicate, child) => FTGuard(predicate, child.map(f))
			case FTLeaf(weights, filter) => FTLeaf(f(weights), filter)
		}
	}

	case class FTConjunction[X, T](children: Seq[FeatureTree[X, T]]) extends FeatureTree[X, T]

	case class FTDispatch[X, T, V](extract: Extractor[X, V],
	                               children: Map[V, FeatureTree[X, T]],
	                               tags: BitSet) extends FeatureTree[X, T]

	case class FTGuard[X, T](predicate: Extractor[X, Boolean], child: FeatureTree[X, T]) extends FeatureTree[X, T]

	case class FTLeaf[X, T](weights: T, tags: BitSet) extends FeatureTree[X, T]

}

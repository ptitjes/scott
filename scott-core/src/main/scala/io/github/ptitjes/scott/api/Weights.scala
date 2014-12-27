package io.github.ptitjes.scott.api

import scala.collection.BitSet

/**
 * @author Didier Villevalois
 */
trait Weights {
	def foreach[U](f: ((Int, Double)) => U): Unit
}

class MutableWeights(val tags: BitSet) extends Weights {
	val values: Array[Double] = Array.ofDim[Double](tags.reduce(math.max) + 1)
	val tagsAsArray = tags.toArray

	def apply(key: Int) = values(key)

	def update(key: Int, value: Double) = values(key) = value

	def foreach[U](f: ((Int, Double)) => U): Unit =
		for (t <- tagsAsArray) {
			f(t, values(t))
		}

	def +=(other: MutableWeights): Unit =
		for (t <- tagsAsArray) {
			values(t) += other.values(t)
		}

	def immutable: ImmutableWeights = ImmutableWeights(tagsAsArray.toList.map(t => (t, values(t))))
}

case class ImmutableWeights(values: List[(Int, Double)]) extends Weights {
	def foreach[U](f: ((Int, Double)) => U): Unit = values.foreach(f)

	def map(f: Double => Double): ImmutableWeights =
		ImmutableWeights(values.map { case (t, w) => (t, f(w))})
}

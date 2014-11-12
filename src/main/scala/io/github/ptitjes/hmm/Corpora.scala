package io.github.ptitjes.hmm

import java.io.File

import scala.collection._
import scala.io.Source

object Corpora {

	case class Corpus[+T <: Sequence](sequences: Seq[T]) {

		def size: Int = sequences.size

		def slice(from: Int, until: Int): Corpus[T] =
			Corpus(sequences.slice(from, until))

		def splitBy(ratio: Double): (Corpus[T], Corpus[T]) = {
			val splitIndex = (size * ratio).toInt
			(slice(0, splitIndex), slice(splitIndex + 1, size))
		}

		def map[U <: Sequence](f: T => U): Corpus[U] =
			Corpus(sequences.map(f))
	}

	trait Sequence {
		def observables: Seq[Int]
	}

	trait Annotation {
		self: Sequence =>
		def states: Seq[Int]

		def observablesAndStates: Seq[(Int, Int)]
	}

	def stateCount(corpus: Corpus[Sequence with Annotation]): Int = {
		var maxState = -1
		corpus.sequences.foreach {
			case seq =>
				seq.observablesAndStates.foreach {
					case (o, s) => if (s > maxState) maxState = s
				}
		}
		maxState + 1
	}

	case class AnnotatedSequence(elements: Seq[(Int, Int)])
		extends Sequence with Annotation {

		def observables: Seq[Int] = elements.map(_._1)

		def states: Seq[Int] = elements.map(_._2)

		def observablesAndStates: Seq[(Int, Int)] = elements
	}

	case class NonAnnotatedSequence(observables: Seq[Int])
		extends Sequence

	def annotatedFrom(source: Source): Corpus[Sequence with Annotation] = {
		val sequences = mutable.ListBuffer[Sequence with Annotation]()

		val elements = mutable.ListBuffer[(Int, Int)]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += AnnotatedSequence(elements.toSeq)
				elements.clear()
			}
			else {
				val split = s.split(' ')
				elements += ((split(0).toInt, split(1).toInt))
			}
		}

		Corpus(sequences)
	}

	def annotatedFrom(file: File): Corpus[Sequence with Annotation] = annotatedFrom(Source.fromFile(file))

	def annotatedFrom(url: java.net.URL): Corpus[Sequence with Annotation] = annotatedFrom(Source.fromURL(url))

	def nonAnnotatedfrom(source: Source): Corpus[Sequence] = {
		val sequences = mutable.ListBuffer[Sequence]()

		val observables = mutable.ListBuffer[Int]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += NonAnnotatedSequence(observables.toSeq)
				observables.clear()
			}
			else {
				val split = s.split(' ')
				observables += split(0).toInt
			}
		}

		Corpus(sequences)
	}

	def nonAnnotatedfrom(file: File): Corpus[Sequence] = nonAnnotatedfrom(Source.fromFile(file))

	def nonAnnotatedfrom(url: java.net.URL): Corpus[Sequence] = nonAnnotatedfrom(Source.fromURL(url))

	def merge[T <: Sequence](c1: Corpus[T], c2: Corpus[T]): Corpus[T] =
		Corpus(c1.sequences ++ c2.sequences)
}

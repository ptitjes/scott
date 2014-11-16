package io.github.ptitjes.hmm

import java.io.File

import io.github.ptitjes.hmm.Features.{Word, History}
import io.github.ptitjes.hmm.Utils._

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
		def observables: IndexedSeq[Int]

		def iterator(breadth: Int, depth: Int) = new HistoryIteratorImpl(this, breadth, depth)
	}

	trait Annotation {
		self: Sequence =>

		def observablesAndStates: IndexedSeq[(Int, Int)]

		def annotatedIterator(breadth: Int, depth: Int) = new AnnotatedHistoryIteratorImpl(this, breadth, depth)
	}

	trait HistoryIterator {

		def hasNext: Boolean

		def next(): Int

		def currentDepth: Int

		def currentObservable: Int

		def history(sourceState: Int): History
	}

	trait AnnotatedHistoryIterator {

		def hasNext: Boolean

		def next(): (Int, Int)

		def currentDepth: Int

		def currentObservable: Int

		def currentTag: Int

		def sourceState: Int

		def history: History
	}

	class HistoryIteratorImpl(val seq: Sequence, val breadth: Int, val depth: Int) extends HistoryIterator {
		private val size = pow(breadth, depth)
		private val observables = seq.observables

		private var _index: Int = -1
		private var _currentDepth: Int = -1

		def hasNext: Boolean = _index < observables.length - 1

		def checkStarted(): Unit =
			if (_index == -1)
				throw new IllegalStateException()

		def next(): Int = {
			if (!hasNext)
				throw new IllegalStateException()

			_index += 1
			if (_currentDepth < depth) {
				_currentDepth += 1
			}

			observables(_index)
		}

		def currentDepth: Int = {
			checkStarted()
			_currentDepth
		}

		def currentObservable: Int = {
			checkStarted()
			observables(_index)
		}

		def history(sourceState: Int): History = {
			checkStarted()

			def makeWord(i: Int) =
				if (i < 0 || i >= observables.length) null
				else {
					val o = observables(i)
					Word(o, Lexica.WORDS(o))
				}

			def makeHistoryTag(i: Int) =
				if (_currentDepth < i) -1
				else {
					var t = sourceState
					for (j <- 1 until i) t /= breadth
					t % breadth
				}

			History(makeWord(_index),
				(1 to depth).map(i => makeWord(_index - i)),
				(1 to depth).map(i => makeWord(_index + i)),
				(1 to depth).map(i => makeHistoryTag(i))
			)
		}
	}

	class AnnotatedHistoryIteratorImpl(val seq: Sequence with Annotation, val breadth: Int, val depth: Int) extends AnnotatedHistoryIterator {
		private val size = pow(breadth, depth)
		private val observablesAndStates = seq.observablesAndStates

		private var _index: Int = -1
		private var _currentDepth: Int = -1
		private var _sourceState = -1

		def hasNext: Boolean = _index < observablesAndStates.length - 1

		def checkStarted(): Unit =
			if (_index == -1)
				throw new IllegalStateException()

		def next(): (Int, Int) = {
			if (!hasNext)
				throw new IllegalStateException()

			if (_index == 0)
				_sourceState = currentTag
			else if (_index > 0)
				_sourceState = (_sourceState * breadth + currentTag) % size

			_index += 1
			if (_currentDepth < depth) {
				_currentDepth += 1
			}

			observablesAndStates(_index)
		}

		def currentDepth: Int = {
			checkStarted()
			_currentDepth
		}

		def currentObservable: Int = {
			checkStarted()
			observablesAndStates(_index)._1
		}

		def currentTag: Int = {
			checkStarted()
			observablesAndStates(_index)._2
		}

		def sourceState: Int = {
			checkStarted()
			_sourceState
		}

		def history: History = {
			checkStarted()

			def makeWord(i: Int) =
				if (i < 0 || i >= observablesAndStates.length) null
				else {
					val o = observablesAndStates(i)._1
					Word(o, Lexica.WORDS(o))
				}

			def makeHistoryTag(i: Int) =
				if (_currentDepth < i) -1
				else {
					var t = _sourceState
					for (j <- 1 until i) t /= breadth
					t % breadth
				}

			History(makeWord(_index),
				(1 to depth).map(i => makeWord(_index - i)),
				(1 to depth).map(i => makeWord(_index + i)),
				(1 to depth).map(i => makeHistoryTag(i))
			)
		}
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

	case class AnnotatedSequence(elements: IndexedSeq[(Int, Int)])
		extends Sequence with Annotation {

		def observables: IndexedSeq[Int] = elements.map(_._1)

		def observablesAndStates: IndexedSeq[(Int, Int)] = elements
	}

	case class NonAnnotatedSequence(observables: IndexedSeq[Int])
		extends Sequence

	def annotatedFrom(source: Source): Corpus[Sequence with Annotation] = {
		val sequences = mutable.ListBuffer[Sequence with Annotation]()

		val elements = mutable.ArrayBuffer[(Int, Int)]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += AnnotatedSequence(elements.toIndexedSeq)
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

		val observables = mutable.ArrayBuffer[Int]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += NonAnnotatedSequence(observables.toIndexedSeq)
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

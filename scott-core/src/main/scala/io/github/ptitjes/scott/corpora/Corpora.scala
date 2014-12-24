package io.github.ptitjes.scott.corpora

import java.io.File

import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott.corpora.Annotation.All
import io.github.ptitjes.scott.utils.States

import scala.collection._
import scala.io.Source

trait Annotation[T]

object Annotation {

	object Form extends Annotation[Word]

	object CoarsePosTag extends Annotation[Int]

	object PosTag extends Annotation[Int]

	type All = Form.type with CoarsePosTag.type
}

trait Corpus {

	def tagSet: TagSet

	def size: Int

	def sequences: Seq[Sentence]

	def slice(from: Int, until: Int): Corpus

	def splitBy(ratio: Double): (Corpus, Corpus)

	def foreach[U](f: Sentence => U): Unit

	def map(f: Sentence => Sentence): Corpus
}

trait Sentence {

	def length: Int = tokens.length

	def tokens: IndexedSeq[Token[Annotation.All]]

	def historyIterator: Iterator[History]

	def zippedHistoryIterator(s: Sentence): Iterator[(History, History)]
}

object Sentence {
	def apply(data: IndexedSeq[Token[Annotation.All]]) = BaseSequence(data)
}

trait Token[+A] {

	def get[AT >: A, T](annotation: AT)(implicit evidence: AT <:< Annotation[T]): T

	def overlay[AT <: Annotation[T], T](annotation: AT, value: T): Token[Annotation.All]
}

trait History {

	def word: Word = current.get(Annotation.Form)

	def wordAt(index: Int): Option[Word] = at(index).map(_.get(Annotation.Form))

	def current: Token[Annotation.All] = at(0).get

	def at(position: Int): Option[Token[Annotation.All]]
}

object Corpora {

	def annotatedFrom(source: Source, lexicon: Lexicon, tagSet: TagSet): Corpus = {
		val sequences = mutable.ListBuffer[Sentence]()

		val elements = mutable.ArrayBuffer[MapToken]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += BaseSequence(elements.toIndexedSeq)
				elements.clear()
			}
			else {
				val split = s.split(' ')
				val word = lexicon(split(0).toInt)
				val tag = split(1).toInt
				elements += MapToken(Map(Annotation.Form -> word, Annotation.CoarsePosTag -> tag))
			}
		}

		BasicCorpus(sequences, tagSet)
	}

	def annotatedFrom(file: File, lexicon: Lexicon, tagSet: TagSet): Corpus =
		annotatedFrom(Source.fromFile(file), lexicon, tagSet)

	def annotatedFrom(url: java.net.URL, lexicon: Lexicon, tagSet: TagSet): Corpus =
		annotatedFrom(Source.fromURL(url), lexicon, tagSet)
}


case class BasicCorpus(sequences: Seq[Sentence], tagSet: TagSet) extends Corpus {

	override def size: Int = sequences.size

	override def slice(from: Int, until: Int): Corpus =
		BasicCorpus(sequences.slice(from, until), tagSet)

	override def splitBy(ratio: Double): (Corpus, Corpus) = {
		val splitIndex = (size * ratio).toInt
		(slice(0, splitIndex), slice(splitIndex + 1, size))
	}

	override def foreach[U](f: Sentence => U): Unit = sequences.foreach(f)

	override def map(f: Sentence => Sentence): Corpus =
		BasicCorpus(sequences.map(f), tagSet)
}

abstract class AbstractSequence extends Sentence {

	override def historyIterator: Iterator[History] = new Iterator[History] {

		val _length = AbstractSequence.this.length
		var index = -1
		val history = new History {
			override def at(position: Int): Option[Token[All]] = {
				val i = index + position
				if (i < 0 || i >= _length) None
				else Some(tokens(i))
			}
		}

		override def next(): History = {
			index += 1
			history
		}

		override def hasNext: Boolean = index < _length - 1
	}

	override def zippedHistoryIterator(s: Sentence): Iterator[(History, History)] = new Iterator[(History, History)] {

		val _length = AbstractSequence.this.length
		var index = -1
		val leftHistory = new History {
			override def at(position: Int): Option[Token[All]] = {
				val i = index + position
				if (i < 0 || i >= _length) None
				else Some(tokens(i))
			}
		}
		val rightHistory = new History {
			override def at(position: Int): Option[Token[All]] = {
				val i = index + position
				if (i < 0 || i >= _length) None
				else Some(s.tokens(i))
			}
		}

		override def next(): (History, History) = {
			index += 1
			(leftHistory, rightHistory)
		}

		override def hasNext: Boolean = index < _length - 1
	}
}

case class BaseSequence(tokens: IndexedSeq[Token[Annotation.All]]) extends AbstractSequence

case class MapToken(data: Map[Annotation[_], Any]) extends Token[Annotation.All] {

	override def get[AT >: All, T](annotation: AT)(implicit evidence: <:<[AT, Annotation[T]]): T =
		data(annotation).asInstanceOf[T]

	def overlay[AT <: Annotation[T], T](annotation: AT, value: T): Token[Annotation.All] =
		MapToken(data + (annotation -> value))
}

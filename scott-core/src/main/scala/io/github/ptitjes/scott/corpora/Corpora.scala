package io.github.ptitjes.scott.corpora

import java.io.File

import scala.collection._
import scala.io.Source

trait Corpus[+Token] {

	def tagSet: TagSet

	def size: Int

	def sequences: Seq[Sentence[Token]]

	def slice(from: Int, until: Int): Corpus[Token]

	def splitBy(ratio: Double): (Corpus[Token], Corpus[Token])

	def foreach[U](f: Sentence[Token] => U): Unit

	def map[OToken](f: Sentence[Token] => Sentence[OToken]): Corpus[OToken]
}

trait Sentence[+Token] {

	def length: Int = tokens.length

	def tokens: IndexedSeq[Token]

	def historyIterator: Iterator[History[Token]]

	def zippedHistoryIterator[O](s: Sentence[O]): Iterator[(History[Token], History[O])]
}

trait History[+Token] {

	def current: Token = at(0).get

	def at(position: Int): Option[Token]
}

object Sentence {
	def apply[Token](data: IndexedSeq[Token]) = BaseSequence(data)
}

trait Annotation[T]

object Annotation {

	object Form extends Annotation[Word]

	object CoarsePosTag extends Annotation[Int]

	object PosTag extends Annotation[Int]

	type All = Form.type with CoarsePosTag.type
}

case class BasicCorpus[Token](sequences: Seq[Sentence[Token]], tagSet: TagSet) extends Corpus[Token] {

	override def size: Int = sequences.size

	override def slice(from: Int, until: Int): Corpus[Token] =
		BasicCorpus(sequences.slice(from, until), tagSet)

	override def splitBy(ratio: Double): (Corpus[Token], Corpus[Token]) = {
		val splitIndex = (size * ratio).toInt
		(slice(0, splitIndex), slice(splitIndex + 1, size))
	}

	override def foreach[U](f: Sentence[Token] => U): Unit = sequences.foreach(f)

	override def map[OToken](f: Sentence[Token] => Sentence[OToken]): Corpus[OToken] =
		BasicCorpus(sequences.map(f), tagSet)
}

abstract class AbstractSequence[Token] extends Sentence[Token] {

	override def historyIterator: Iterator[History[Token]] = new Iterator[History[Token]] {

		val _length = AbstractSequence.this.length
		var index = -1
		val history = new History[Token] {
			override def at(position: Int): Option[Token] = {
				val i = index + position
				if (i < 0 || i >= _length) None
				else Some(tokens(i))
			}
		}

		override def next(): History[Token] = {
			index += 1
			history
		}

		override def hasNext: Boolean = index < _length - 1
	}

	override def zippedHistoryIterator[O](s: Sentence[O]): Iterator[(History[Token], History[O])] =
		new Iterator[(History[Token], History[O])] {

			val _length = AbstractSequence.this.length
			var index = -1
			val leftHistory = new History[Token] {
				override def at(position: Int): Option[Token] = {
					val i = index + position
					if (i < 0 || i >= _length) None
					else Some(tokens(i))
				}
			}
			val rightHistory = new History[O] {
				override def at(position: Int): Option[O] = {
					val i = index + position
					if (i < 0 || i >= _length) None
					else Some(s.tokens(i))
				}
			}

			override def next(): (History[Token], History[O]) = {
				index += 1
				(leftHistory, rightHistory)
			}

			override def hasNext: Boolean = index < _length - 1
		}
}

case class BaseSequence[Token](tokens: IndexedSeq[Token]) extends AbstractSequence[Token]

trait NLToken {
	def word: Word
}

trait NLCoarsePosTag {
	def coarseTag: Int
}

trait NLPosTag {
	def tag: Int
}

case class AnnotatedNLToken(word: Word, tag: Int) extends NLToken with NLPosTag

object Corpora {

	def annotatedFrom(source: Source, lexicon: Lexicon, tagSet: TagSet): Corpus[NLToken with NLPosTag] = {
		val sequences = mutable.ListBuffer[Sentence[NLToken with NLPosTag]]()

		val elements = mutable.ArrayBuffer[NLToken with NLPosTag]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += BaseSequence(elements.toIndexedSeq)
				elements.clear()
			} else {
				val split = s.split(' ')
				val word = lexicon(split(0).toInt)
				val tag = split(1).toInt
				elements += AnnotatedNLToken(word, tag)
			}
		}

		BasicCorpus(sequences, tagSet)
	}

	def annotatedFrom(file: File, lexicon: Lexicon, tagSet: TagSet): Corpus[NLToken with NLPosTag] =
		annotatedFrom(Source.fromFile(file), lexicon, tagSet)

	def annotatedFrom(url: java.net.URL, lexicon: Lexicon, tagSet: TagSet): Corpus[NLToken with NLPosTag] =
		annotatedFrom(Source.fromURL(url), lexicon, tagSet)
}

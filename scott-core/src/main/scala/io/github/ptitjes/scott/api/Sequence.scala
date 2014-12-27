package io.github.ptitjes.scott.api

import scala.collection.{IndexedSeq, Iterator}

/**
 * @author Didier Villevalois
 */
object Sequence {
	def apply[Token](data: IndexedSeq[Token]) = BaseSequence(data)
}

trait Sequence[+Token] {

	def length: Int = tokens.length

	def tokens: IndexedSeq[Token]

	def historyIterator: Iterator[History[Token]]

	def zippedHistoryIterator[O](s: Sequence[O]): Iterator[(History[Token], History[O])]
}

trait History[+Token] {

	def current: Token = at(0).get

	def at(position: Int): Option[Token]
}

case class BaseSequence[Token](tokens: IndexedSeq[Token]) extends Sequence[Token] {

	override def historyIterator: Iterator[History[Token]] =
		new Iterator[History[Token]] {

			val _length = BaseSequence.this.length
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

	override def zippedHistoryIterator[O](s: Sequence[O]): Iterator[(History[Token], History[O])] =
		new Iterator[(History[Token], History[O])] {

			val _length = BaseSequence.this.length
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

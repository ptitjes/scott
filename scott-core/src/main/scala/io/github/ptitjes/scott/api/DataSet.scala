package io.github.ptitjes.scott.api

import scala.collection._

object DataSet {
	def apply[Token](sequences: Seq[Sequence[Token]], tagSet: TagSet) =
		BaseDataSet(sequences, tagSet)
}

trait DataSet[+Token] {

	def tagSet: TagSet

	def size: Int

	def sequences: Seq[Sequence[Token]]

	def slice(from: Int, until: Int): DataSet[Token]

	def splitBy(ratio: Double): (DataSet[Token], DataSet[Token])

	def foreach[U](f: Sequence[Token] => U): Unit

	def map[OToken](f: Sequence[Token] => Sequence[OToken]): DataSet[OToken]
}

case class BaseDataSet[Token](sequences: Seq[Sequence[Token]], tagSet: TagSet) extends DataSet[Token] {

	override def size: Int = sequences.size

	override def slice(from: Int, until: Int): DataSet[Token] =
		BaseDataSet(sequences.slice(from, until), tagSet)

	override def splitBy(ratio: Double): (DataSet[Token], DataSet[Token]) = {
		val splitIndex = (size * ratio).toInt
		(slice(0, splitIndex), slice(splitIndex + 1, size))
	}

	override def foreach[U](f: Sequence[Token] => U): Unit = sequences.foreach(f)

	override def map[OToken](f: Sequence[Token] => Sequence[OToken]): DataSet[OToken] =
		BaseDataSet(sequences.map(f), tagSet)
}

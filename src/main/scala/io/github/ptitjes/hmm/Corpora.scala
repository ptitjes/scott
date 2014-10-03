package io.github.ptitjes.hmm

import java.io.File

import scala.io.Source

object Corpora {

  trait CorpusItem

  case class WordCategoryPair(word: Int, cat: Int) extends CorpusItem

  case object SentenceSeparator extends CorpusItem

  def fromFile(file: File): Iterator[CorpusItem] = {
    Source.fromFile(file).getLines().map { s =>
      if (s.isEmpty) SentenceSeparator
      else {
        val split = s.split(' ')
        WordCategoryPair(split(0).toInt, split(1).toInt)
      }
    }
  }
}

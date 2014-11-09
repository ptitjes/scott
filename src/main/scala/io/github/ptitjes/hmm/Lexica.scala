package io.github.ptitjes.hmm

import java.io.File

import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source

object Lexica {

  val WORDS = from(getClass.getResource("/data/vocabulary"))
  val CATEGORIES = from(getClass.getResource("/data/categories"))

  case class Lexicon(elements: Seq[String], maxLength: Int) {

    def apply(i: Int) = elements(i)

    def padded(i: Int) = {
      val element = elements(i)
      element + " " * (maxLength - element.length)
    }
  }

  def from(source: Source): Lexicon = {
    var maxLength = 0
    val elements  = ListBuffer[String]()
    source.getLines().foreach { s =>
      if (s.length > maxLength) maxLength = s.length
      elements += s
    }
    Lexicon(elements, maxLength)
  }

  def from(file: File): Lexicon = from(Source.fromFile(file))

  def from(url: java.net.URL): Lexicon = from(Source.fromURL(url))

}

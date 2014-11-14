package io.github.ptitjes.hmm

import java.io.File

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source

object Lexica {

	val WORDS = from(getClass.getResource("/data/vocabulary"))
	val CATEGORIES = from(getClass.getResource("/data/categories"))

	object Categories {
		val A = 0
		val ADV = 1
		val C = 2
		val CL = 3
		val D = 4
		val ET = 5
		val I = 6
		val N = 7
		val P = 8
		val P_D = 9
		val PONCT = 10
		val P_PRO = 11
		val PREF = 12
		val PRO = 13
		val V = 14
	}

	case class Lexicon(elements: IndexedSeq[String], maxLength: Int) {

		def apply(i: Int) = elements(i)

		def padded(i: Int) = {
			val element = elements(i)
			element + " " * (maxLength - element.length)
		}
	}

	def from(source: Source): Lexicon = {
		var maxLength = 0
		val elements = ArrayBuffer[String]()
		source.getLines().foreach { s =>
			if (s.length > maxLength) maxLength = s.length
			elements += s
		}
		Lexicon(elements, maxLength)
	}

	def from(file: File): Lexicon = from(Source.fromFile(file))

	def from(url: java.net.URL): Lexicon = from(Source.fromURL(url))

}

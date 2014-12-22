package io.github.ptitjes.scott

import java.io.File

import io.github.ptitjes.scott.corpora.TagSet
import io.github.ptitjes.scott.utils.Trie

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Lexica {

	val WORDS = from(getClass.getResource("/data/vocabulary"))
	val CATEGORIES = TagSet(
		IndexedSeq(
			"A",
			"ADV",
			"C",
			"CL",
			"D",
			"ET",
			"I",
			"N",
			"P",
			"P+D",
			"P+PRO",
			"PONCT",
			"PREF",
			"PRO",
			"V"
		)
	)

	object Categories {
		val A = CATEGORIES("A")
		val ADV = CATEGORIES("ADV")
		val C = CATEGORIES("C")
		val CL = CATEGORIES("CL")
		val D = CATEGORIES("D")
		val ET = CATEGORIES("ET")
		val I = CATEGORIES("I")
		val N = CATEGORIES("N")
		val P = CATEGORIES("P")
		val P_D = CATEGORIES("P+D")
		val P_PRO = CATEGORIES("P+PRO")
		val PONCT = CATEGORIES("PONCT")
		val PREF = CATEGORIES("PREF")
		val PRO = CATEGORIES("PRO")
		val V = CATEGORIES("V")
	}

	case class Lexicon(elements: IndexedSeq[String], maxLength: Int) {

		private val stringToWord = Trie[Word]() ++ elements.zipWithIndex.map {
			case (s, i) => (s, Word(i, s))
		}

		def apply(i: Int) = elements(i)

		def apply(s: String) = stringToWord(s).get.code

		def get(i: Int) = Word(i, elements(i))

		def get(s: String) = stringToWord(normalizeWord(s)).getOrElse(Word(-1, s))

		def words = (0 until elements.length).map(i => get(i))

		def padded(i: Int) = {
			val element = elements(i)
			element + " " * (maxLength - element.length)
		}
	}

	def normalizeWord(string: String) =
		string.replace("'_", "'").replace("_-_", "-").replace("_,_", "-").replace('_', ' ')

	def from(source: Source): Lexicon = {
		var maxLength = 0
		val elements = ArrayBuffer[String]()
		source.getLines().foreach { s =>
			if (s.length > maxLength) maxLength = s.length
			elements += normalizeWord(s)
		}
		Lexicon(elements, maxLength)
	}

	def from(file: File): Lexicon = from(Source.fromFile(file))

	def from(url: java.net.URL): Lexicon = from(Source.fromURL(url))

}

case class Word(code: Int, string: String)

package io.github.ptitjes.scott.nl.corpora

import java.io.File

import io.github.ptitjes.scott.api.TagSet
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

	case class BasicLexicon(words: Seq[Word]) extends Lexicon {

		private val codeToWord = Map[Int, Word]() ++ words.map(w => (w.code, w))
		private val stringToWord = Trie[Word]() ++ words.map(w => (w.string, w))
		private val maxLength = words.map(_.string.length).reduce(math.max)

		def apply(i: Int) = codeToWord(i)

		def apply(s: String) = {
			val string = normalizeWord(s)
			stringToWord(string).getOrElse(BasicWord(-1, string))
		}

		def padded(i: Int) = {
			val element = codeToWord(i).string
			element + " " * (maxLength - element.length)
		}

		def size = words.size
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
		BasicLexicon(elements.zipWithIndex.map {
			case ((string, code)) => BasicWord(code, string)
		})
	}

	def from(file: File): Lexicon = from(Source.fromFile(file))

	def from(url: java.net.URL): Lexicon = from(Source.fromURL(url))

}

trait Lexicon {

	def apply(i: Int): Word

	def apply(s: String): Word

	def padded(i: Int): String

	def words: Seq[Word]

	def size: Int
}

trait Word {
	def string: String
	def code: Int
}

case class BasicWord(code: Int, string: String) extends Word

package io.github.ptitjes.scott.nl.lang.fr

import java.nio.charset.Charset
import java.util.Locale

import io.github.ptitjes.scott.api.Sequence
import io.github.ptitjes.scott.nl.conll.CoNLLToken
import io.github.ptitjes.scott.nl.corpora._
import io.github.ptitjes.scott.nl.lang.fr.Lefff.LefffTags
import io.github.ptitjes.scott.nl.lang.fr.testFTBCoarse._
import io.github.ptitjes.scott.utils.Trie

import scala.collection.{mutable, _}
import scala.io.Source

object testLefff extends App {

	val ftbPath = args(0)
	val lefffPath = args(1)

	val corpus = FTB.parseFullFine(ftbPath)

	val perWordCats = mutable.HashMap[Word, mutable.Set[Int]]()
	corpus.sequences.foreach { s =>
		s.tokens.foreach { t =>
			if (perWordCats.contains(t.word)) {
				perWordCats(t.word) += t.tag
			} else {
				perWordCats(t.word) = mutable.Set(t.tag)
			}
		}
	}

	val trie = Lefff.parse(lefffPath, "3.2")

	val allTags = mutable.Set[Int]()
	Lexica.WORDS.words.foreach {
		word =>

//			if (trie(word.string).isEmpty) println(word.string)

			val lefffWord = toLefffWord(word)
			val lefffTags = trie(lefffWord.toLowerCase(Locale.FRENCH)).getOrElse(Set()) ++
				trie(lefffWord).getOrElse(Set())
			val localTags = if (perWordCats.contains(word)) perWordCats(word) else Set[Int]()
			val convertedLefffTags = lefffTags.map(fromLefffTag)

			if (!localTags.subsetOf(convertedLefffTags) && trie(lefffWord) != None) {
				println(word.string)
				println(lefffTags.mkString("[", ", ", "]"))
				println(localTags.map(formatCat).mkString("[", ", ", "]"))
				println(convertedLefffTags.map(formatCat).mkString("[", ", ", "]"))
				println()

				(localTags -- convertedLefffTags).foreach {
					t =>
						val sequence = findSequenceWithWordAs(word, t)
						printAnnotatedSequence(sequence)
						println()
				}
			}
			allTags ++= lefffTags
	}

	def printAnnotatedSequence(sequence: Sequence[CoNLLToken]) {
		var sentence = ""
		var tags = ""
		sequence.tokens.foreach { t =>
			val cat = formatCat(t.tag)
			val word = t.word.string
			val length = math.max(cat.length, word.length)
			sentence += word + " " * (length - word.length + 1)
			tags += cat + " " * (length - cat.length + 1)
		}

		println(sentence)
		println(tags)
	}

	println()
	allTags.toList.sorted.map(c => (c, formatCat(fromLefffTag(c)))).foreach(p => println(p))

	def toLefffWord(s: Word): String =
		s.string.replace("'_", "'").replace("_-_", "-").replace('_', ' ')

	def fromLefffTag(c: Int) = LefffTags(c) match {
		case "adj" => FTB.PosTags("ADJ")
		case "adv" | "advm" | "advp" => FTB.PosTags("ADV")
		case "csu" => FTB.PosTags("CS")
		case "coo" => FTB.PosTags("CC")
		case "cla" | "cld" | "cll" | "cln" | "clr" => FTB.PosTags("CLO")
		case "det" => FTB.PosTags("DET")
		case "cfi" => FTB.PosTags("I")
		case "nc" => FTB.PosTags("NC")
		case "np" => FTB.PosTags("NPP")
		case "prep" => FTB.PosTags("P")
		case "ponctw" | "poncts" | "parento" | "parentf" | "epsilon" => FTB.PosTags("PONCT")
		case "pro" => FTB.PosTags("PRO")
		case "v" | "auxAvoir" | "auxEtre" => FTB.PosTags("V")
		case _ => -1
	}

	def findSequenceWithWordAs(word: Word, tag: Int): Sequence[CoNLLToken] = {
		corpus.sequences.foreach { s =>
			s.tokens.foreach { t =>
				if (word == t.word && tag == t.tag) return s
			}
		}
		null
	}

	def formatCat(c: Int) = if (c != -1) FTB.PosTags(c) else "-1"

	def parse(source: Source): Map[String, Set[String]] = {
		val elements = mutable.HashMap[String, mutable.Set[String]]()
		source.getLines().foreach {
			case l if !l.isEmpty =>
				val split = l.split('\t')
				val word = split(0).toLowerCase(Locale.FRENCH)
				val tag = split(2)
				if (elements.contains(word)) {
					elements(word) += tag
				} else {
					elements(word) = mutable.Set(tag)
				}
			case _ =>
		}
		elements
	}

	def parse(url: java.net.URL): Map[String, Set[String]] = parse(Source.fromURL(url)(Charset forName "ISO-8859-15"))
}

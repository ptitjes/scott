package io.github.ptitjes.scott.corpora

import java.io.File
import java.nio.charset.Charset
import java.util.Locale

import Corpora._
import io.github.ptitjes.scott.utils.Trie

import scala.collection._
import scala.io.Source

object Lefff extends App {

//	private val PATH_TO_TEST = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/ftb.test.encode"
//	val corpus = Corpora.merge(Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS),
//		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS),
//		Corpora.annotatedFrom(new File(PATH_TO_TEST), Lexica.WORDS))
//
//	val perWordCats = mutable.HashMap[Word, mutable.Set[Int]]()
//	corpus.sequences.foreach { s =>
//		s.observablesAndStates.foreach { case (word, tag) =>
//			if (perWordCats.contains(word)) {
//				perWordCats(word) += tag
//			} else {
//				perWordCats(word) = mutable.Set(tag)
//			}
//		}
//	}
//
//	val p = parse(getClass.getResource("/lefff/lefff-ext-3.2.txt"))
//	val trie = Trie[Set[String]]() ++ p
//
//	val allTags = mutable.Set[String]()
//	Lexica.WORDS.words.foreach {
//		word =>
//			val lefffWord = toLefffWord(word)
//			val lefffTags = trie(lefffWord.toLowerCase(Locale.FRENCH)).getOrElse(Set()) ++
//				trie(lefffWord).getOrElse(Set())
//			val localTags = if (perWordCats.contains(word)) perWordCats(word) else Set[Int]()
//			val convertedLefffTags = lefffTags.map(fromLefffTag)
//
//			if (!localTags.subsetOf(convertedLefffTags) && trie(lefffWord) != None) {
//				println(word.string)
//				println(lefffTags.mkString("[", ", ", "]"))
//				println(localTags.map(formatCat).mkString("[", ", ", "]"))
//				println(convertedLefffTags.map(formatCat).mkString("[", ", ", "]"))
//				println()
//
//				(localTags -- convertedLefffTags).foreach {
//					t =>
//						val sequence = findSequenceWithWordAs(word, t)
//						printAnnotatedSequence(sequence)
//						println()
//				}
//			}
//			allTags ++= lefffTags
//	}
//
//	def printAnnotatedSequence(sequence: Sequence with Annotation) {
//		var sentence = ""
//		var tags = ""
//		sequence.observablesAndStates.foreach { case (w, t) =>
//			val cat = formatCat(t)
//			val word = w.string
//			val length = math.max(cat.length, word.length)
//			sentence += word + " " * (length - word.length + 1)
//			tags += cat + " " * (length - cat.length + 1)
//		}
//
//		println(sentence)
//		println(tags)
//	}
//
//	println()
//	allTags.toList.sorted.map(c => (c, formatCat(fromLefffTag(c)))).foreach(p => println(p))
//
//	def toLefffWord(s: Word): String =
//		s.string.replace("'_", "'").replace("_-_", "-").replace('_', ' ')
//
//	def fromLefffTag(c: String) = c match {
//		case "adj" => Lexica.Categories.A
//		case "adv" | "advm" | "advp" => Lexica.Categories.ADV
//		case "csu" | "coo" => Lexica.Categories.C
//		case "cla" | "cld" | "cll" | "cln" | "clr" => Lexica.Categories.CL
//		case "det" => Lexica.Categories.D
//		case "cfi" => Lexica.Categories.I
//		case "nc" | "np" => Lexica.Categories.N
//		case "prep" => Lexica.Categories.P
//		case "ponctw" | "poncts" | "parento" | "parentf" | "epsilon" => Lexica.Categories.PONCT
//		case "pro" => Lexica.Categories.PRO
//		case "v" | "auxAvoir" | "auxEtre" => Lexica.Categories.V
//		case _ => -1
//	}
//
//	def findSequenceWithWordAs(word: Word, tag: Int): Sequence with Annotation = {
//		corpus.sequences.foreach { s =>
//			s.observablesAndStates.foreach { case (w, t) =>
//				if (word == w && tag == t) return s
//			}
//		}
//		null
//	}
//
//	def formatCat(c: Int) = if (c != -1) Lexica.CATEGORIES(c) else "-1"
//
//	def parse(source: Source): Map[String, Set[String]] = {
//		val elements = mutable.HashMap[String, mutable.Set[String]]()
//		source.getLines().foreach {
//			case l if !l.isEmpty =>
//				val split = l.split('\t')
//				val word = split(0).toLowerCase(Locale.FRENCH)
//				val tag = split(2)
//				if (elements.contains(word)) {
//					elements(word) += tag
//				} else {
//					elements(word) = mutable.Set(tag)
//				}
//			case _ =>
//		}
//		elements
//	}
//
//	def parse(url: java.net.URL): Map[String, Set[String]] = parse(Source.fromURL(url)(Charset forName "ISO-8859-15"))
}

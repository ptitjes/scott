package io.github.ptitjes.scott.nl.conll

import io.github.ptitjes.scott.corpora.{TagSet, Corpora}
import io.github.ptitjes.scott.{Word, Lexica}
import Corpora._

import scala.collection.mutable
import scala.io.Source

/**
 * @author Didier Villevalois
 */
class ConllParser {
	
	import ConllParser._

	def parse(profile: Profile, source: Source, lexicon: Lexica.Lexicon): Corpus[Sequence with Annotation] = {
		val sequences = mutable.ListBuffer[Sequence with Annotation]()

		val elements = mutable.ArrayBuffer[(Word, Int)]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += AnnotatedSequence(elements.toIndexedSeq)
				elements.clear()
			}
			else {
				val split = s.split('\t')
				val word = lexicon.get(split(profile.wordColumn))
				val tag = profile.tagSet(split(profile.tagColumn))
				elements += ((word, tag))
			}
		}

		Corpus(sequences)
	}
}

object ConllParser {

	case class Profile(wordColumn: Int, lemmaColumn: Int, tagColumn: Int, tagSet: TagSet)

}

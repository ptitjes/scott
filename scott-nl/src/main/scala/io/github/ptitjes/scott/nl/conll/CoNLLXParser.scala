package io.github.ptitjes.scott.nl.conll

import io.github.ptitjes.scott.corpora._

import scala.collection.mutable
import scala.io.Source

/**
 * @author Didier Villevalois
 */
class CoNLLXParser {

	import io.github.ptitjes.scott.nl.conll.CoNLLXParser._

	def parse(profile: Profile, source: Source, lexicon: Lexicon): Corpus[Sequence with Annotation] = {
		val sequences = mutable.ListBuffer[Sequence with Annotation]()

		val elements = mutable.ArrayBuffer[(Word, Int)]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += AnnotatedSequence(elements.toIndexedSeq)
				elements.clear()
			}
			else {
				val split = s.split('\t')
				val word = lexicon(split(profile.wordColumn))
				val tag = profile.tagSet(split(profile.tagColumn))
				elements += ((word, tag))
			}
		}

		BasicCorpus(sequences, profile.tagSet)
	}
}

object CoNLLXParser {

	case class Profile(wordColumn: Int, lemmaColumn: Int, tagColumn: Int, tagSet: TagSet)

}

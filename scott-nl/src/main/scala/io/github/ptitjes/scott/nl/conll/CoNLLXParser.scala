package io.github.ptitjes.scott.nl.conll

import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.nl.corpora._
import io.github.ptitjes.scott.nl.corpora.Corpora._

import scala.collection.mutable
import scala.io.Source

/**
 * @author Didier Villevalois
 */
class CoNLLXParser {

	import io.github.ptitjes.scott.nl.conll.CoNLLXParser._

	def parse(profile: Profile, source: Source, lexicon: Lexicon): DataSet[CoNLLToken] = {
		val sequences = mutable.ListBuffer[Sequence[CoNLLToken]]()

		val elements = mutable.ArrayBuffer[CoNLLToken]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += BaseSequence(elements.toIndexedSeq)
				elements.clear()
			} else {
				val split = s.split('\t')
				val word = lexicon(split(1))
				val coarseTag = profile.coarseTagSet(split(3))
				val tag = profile.tagSet(split(4))
				elements += CoNLLToken(word, coarseTag, tag)
			}
		}

		BaseDataSet(sequences, profile.tagSet)
	}

	def parseCoarse(profile: Profile, source: Source, lexicon: Lexicon): DataSet[CoNLLCoarseToken] = {
		val sequences = mutable.ListBuffer[Sequence[CoNLLCoarseToken]]()

		val elements = mutable.ArrayBuffer[CoNLLCoarseToken]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += BaseSequence(elements.toIndexedSeq)
				elements.clear()
			} else {
				val split = s.split('\t')
				val word = lexicon(split(1))
				val coarseTag = profile.coarseTagSet(split(3))
				elements += CoNLLCoarseToken(word, coarseTag)
			}
		}

		BaseDataSet(sequences, profile.coarseTagSet)
	}
}

case class CoNLLToken(word: Word, coarseTag: Int, tag: Int) extends NLToken with NLCoarsePosTag with NLPosTag

case class CoNLLCoarseToken(word: Word, tag: Int) extends NLToken with NLPosTag

object CoNLLToken {

}

object CoNLLXParser {

	case class Profile(coarseTagSet: TagSet, tagSet: TagSet)

}

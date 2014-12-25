package io.github.ptitjes.scott.nl.conll

import io.github.ptitjes.scott.corpora._

import scala.collection.mutable
import scala.io.Source

/**
 * @author Didier Villevalois
 */
class CoNLLXParser {

	import io.github.ptitjes.scott.nl.conll.CoNLLXParser._

	def parse(profile: Profile, source: Source, lexicon: Lexicon): Corpus[CoNLLToken] = {
		val sequences = mutable.ListBuffer[Sentence[CoNLLToken]]()

		val elements = mutable.ArrayBuffer[CoNLLToken]()
		source.getLines().foreach { s =>
			if (s.isEmpty) {
				sequences += BaseSequence[CoNLLToken](elements.toIndexedSeq)
				elements.clear()
			} else {
				val split = s.split('\t')
				val word = lexicon(split(1))
				val coarseTag = profile.coarseTagSet(split(3))
				val tag = profile.tagSet(split(4))
				elements += CoNLLToken(word, coarseTag, tag)
			}
		}

		BasicCorpus(sequences, profile.tagSet)
	}
}

case class CoNLLToken(word: Word, coarseTag: Int, tag: Int) extends NLToken with NLCoarsePosTag with NLPosTag

object CoNLLToken {

}

/*

case class CoNLLToken(form: Word, coarseTag: Int, tag: Int) extends Token[Annotation.All] {
	override def get[AT >: All, T](annotation: AT)(implicit evidence: <:<[AT, Annotation[T]]): T = annotation match {
		case Annotation.Form => form.asInstanceOf[T]
		case Annotation.CoarsePosTag => coarseTag.asInstanceOf[T]
		case Annotation.PosTag => tag.asInstanceOf[T]
	}
}
*/

object CoNLLXParser {

	case class Profile(coarseTagSet: TagSet, tagSet: TagSet)

}

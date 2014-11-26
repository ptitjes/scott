package io.github.ptitjes.hmm.trainers

import io.github.ptitjes.hmm.Corpora.{Annotation, Sequence, Corpus}
import io.github.ptitjes.hmm.Features._

import scala.collection.{mutable, Map, BitSet, IndexedSeq}

case class FeatureTemplate(items: List[Extractor[_]], isUnleashed: Boolean = false) {
	def unleash = FeatureTemplate(items, isUnleashed = true)
}

object FeatureTemplate {

	def apply(items: Extractor[_]*): FeatureTemplate = FeatureTemplate(items.toList)

	def apply(items: IndexedSeq[Extractor[_]]): FeatureTemplate = FeatureTemplate(items.toList)
}

trait FeatureSetTemplate {

	def name: String

	def features(order: Int): List[FeatureTemplate]

	def w(i: Int) = EWordCode(i)

	def t(i: Int) = EPreviousTag(i)

	def p(i: Int) = EPrefixChar(i)

	def s(i: Int) = ESuffixChar(i)

	def not(predicate: Predicate) = PNot(predicate)

	implicit class WordFeatureItem(wordItem: EWordCode) {

		def contains(char: Char) = PContains(EWordString(wordItem.index), char)

		def containsUppercase = PContainsUppercase(EWordString(wordItem.index))

		def containsOnlyUppercase = PUppercaseOnly(EWordString(wordItem.index))

		def containsNumber = PContainsNumber(EWordString(wordItem.index))
	}

	implicit class RichTagFeatureItem(tagItem: EPreviousTag) {

		def ===(tag: Int) = PTagEqual(tagItem, tag)
	}

	def buildFeatures[T](breadth: Int, order: Int,
	                     corpus: Corpus[Sequence with Annotation],
	                     f: BitSet => T): (FeatureTree[T], FeatureTree[T], Map[Int, BitSet]) = {

		val templates = features(order)

		val allData = templates.map {
			template => (template, mutable.Map[List[_], BitSet]())
		}.toMap
		val dictionary = mutable.Map[Int, BitSet]()

		corpus.sequences.foreach { s: Sequence with Annotation =>
			val iterator = s.annotatedIterator(breadth, order)
			while (iterator.hasNext) {
				val (word, tag) = iterator.next()
				val history = iterator.history

				val code = word.code
				if (!dictionary.contains(code))
					dictionary(code) = BitSet(tag)
				else
					dictionary(code) += tag

				templates.foreach { template =>
					val combination = template.items.map(item => item(history))
					if (!combination.contains(null) &&
						!combination.contains(false) &&
						!combination.contains(0.toChar)) {
						val data = allData(template)
						if (!data.contains(combination))
							data(combination) = BitSet(tag)
						else
							data(combination) += tag
					}
				}
			}
		}

		val allTags = BitSet() ++ (0 until breadth)

		val wordOnlyFeatures = FTConjunction(
			templates.filter(isWordOnly(_))
				.map(template => generateFrom(template, allData(template), allTags, f))
		)
		val otherFeatures = FTConjunction(
			templates.filter(!isWordOnly(_))
				.map(template => generateFrom(template, allData(template), allTags, f))
		)
		(wordOnlyFeatures, otherFeatures, dictionary)
	}

	private def isWordOnly(template: FeatureTemplate) =
		template.items.forall {
			case EPreviousTag(_) => false
			case _ => true
		}

	private def generateFrom[T](template: FeatureTemplate,
	                            data: Map[List[_], BitSet],
	                            allTags: BitSet,
	                            f: (BitSet) => T): FeatureTree[T] = {
		val unleashed = template.isUnleashed

		def aux(items: List[Extractor[_]], data: Map[List[_], BitSet]): (FeatureTree[T], BitSet) = {
			if (items.isEmpty) {
				val filterTags =
					if (unleashed) allTags
					else data.foldLeft(BitSet()) { case (collected, (_, tags)) => collected ++ tags}

				(FTLeaf(f(filterTags), filterTags), filterTags)
			} else {
				val perPrevious = data.groupBy(_._1.head).mapValues(
					_.map { case (seq, tags) => (seq.tail, tags)}
				)
				val valueToTreeTags = perPrevious.map { case (tag, inner) => (tag, aux(items.tail, inner))}
				val valueToTree = valueToTreeTags.map { case (value, (tree, tags)) => value -> tree}
				val filterTags =
					if (unleashed) allTags
					else valueToTreeTags.foldLeft(BitSet()) { case (collected, (_, (_, tags))) => collected ++ tags}

				val extractor = items.head
				extractor match {
					case PContainsUppercase(_) | PUppercaseOnly(_) | PContainsNumber(_) | PContains(_, _) |
					     PTagEqual(_, _) | PNot(_) =>
						(FTGuard(
							extractor.asInstanceOf[Extractor[Boolean]],
							valueToTree(true)
						), filterTags)
					case EPrefixChar(_) | ESuffixChar(_) =>
						(FTDispatchChar(
							extractor.asInstanceOf[Extractor[Char]],
							valueToTree.asInstanceOf[Map[Char, FeatureTree[T]]],
							filterTags
						), filterTags)
					case EPreviousTag(_) | EWordCode(_) =>
						(FTDispatchInt(
							extractor.asInstanceOf[Extractor[Int]],
							valueToTree.asInstanceOf[Map[Int, FeatureTree[T]]],
							filterTags
						), filterTags)
					case EWordString(_) => throw new UnsupportedOperationException
				}
			}
		}

		val (resultTree, _) = aux(template.items, data)
		resultTree
	}
}

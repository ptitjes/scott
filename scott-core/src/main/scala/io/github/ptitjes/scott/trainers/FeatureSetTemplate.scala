package io.github.ptitjes.scott.trainers

import io.github.ptitjes.scott.corpora.Annotation.{CoarsePosTag, Form}
import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.Features._
import io.github.ptitjes.scott.utils.States.SourceTracker

import scala.annotation.tailrec
import scala.collection._

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
	                     corpus: Corpus,
	                     f: BitSet => T): (FeatureTree[T], FeatureTree[T], Map[Int, BitSet]) = {

		val templates = features(order)

		val allData = templates.map {
			template => (template, mutable.Map[List[_], BitSet]())
		}.toMap
		val dictionary = mutable.Map[Int, BitSet]()

		val source = new SourceTracker(breadth, order)

		corpus.foreach { sequence =>

			val iterator = sequence.historyIterator
			while (iterator.hasNext) {
				val history = iterator.next()

				val (word, tag) = (history.current.get(Form), history.current.get(CoarsePosTag))
				val previousTags = source.tags

				val code = word.code
				if (!dictionary.contains(code))
					dictionary(code) = BitSet(tag)
				else
					dictionary(code) += tag

				templates.foreach { template =>
					val combination = template.items.map(_(history, previousTags))
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

				source.append(tag)
			}

			source.reset()
		}

		val allTags = BitSet() ++ (0 until breadth)

		def generateAll(filter: FeatureTemplate => Boolean): FeatureTree[T] =
			FTConjunction(
				templates.filter(filter)
					.map(template => generateFrom(template, allData(template), allTags, f))
					.foldLeft(List[FeatureTree[T]]())(merge)
			)

		val wordOnlyFeatures = generateAll(isWordOnly)
		val otherFeatures = generateAll(!isWordOnly(_))
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
					case _ => throw new UnsupportedOperationException
				}
			}
		}

		val (resultTree, _) = aux(template.items, data)
		resultTree
	}

	private def merge[T](trees: List[FeatureTree[T]],
	                     toMerge: FeatureTree[T]): List[FeatureTree[T]] = {

		def tryToMergeTrees(left: FeatureTree[T], right: FeatureTree[T]): Option[FeatureTree[T]] =
			(left, right) match {
				case (FTDispatchChar(lExtractor, lChildren, lFilter), FTDispatchChar(rExtractor, rChildren, rFilter))
					if lExtractor == rExtractor =>
					Some(FTDispatchChar(lExtractor, mergeFeatureMaps(lChildren, rChildren), lFilter ++ rFilter))
				case (FTDispatchInt(lExtractor, lChildren, lFilter), FTDispatchInt(rExtractor, rChildren, rFilter))
					if lExtractor == rExtractor =>
					Some(FTDispatchInt(lExtractor, mergeFeatureMaps(lChildren, rChildren), lFilter ++ rFilter))
				case (FTGuard(lPredicate, lChild), FTGuard(rPredicate, rChild))
					if lPredicate == rPredicate =>
					Some(FTGuard(lPredicate, mergeTrees(lChild, rChild)))
				case _ => None
			}

		def mergeFeatureMaps[K](left: Map[K, FeatureTree[T]], right: Map[K, FeatureTree[T]]): Map[K, FeatureTree[T]] =
			right.foldLeft(left) {
				case (result, (key, tree)) =>
					result + (key ->
						(if (result.contains(key)) mergeTrees(result(key), tree)
						else tree))
			}

		def mergeTrees(tree: FeatureTree[T], toMerge: FeatureTree[T]): FeatureTree[T] =
			tree match {
				case FTConjunction(lChildren) => FTConjunction(merge(lChildren.toList, toMerge))
				case _ =>
					val merged = merge(List(tree), toMerge)
					if (merged.size == 1) merged.head
					else FTConjunction(merged)
			}

		@tailrec def aux(yetToTry: List[FeatureTree[T]],
		                 tried: List[FeatureTree[T]]): List[FeatureTree[T]] =
			yetToTry match {
				case Nil => toMerge :: tried
				case toTry :: tail => tryToMergeTrees(toTry, toMerge) match {
					case Some(merged) => merged :: tail ++ tried
					case None => aux(tail, toTry :: tried)
				}
			}

		toMerge match {
			case FTConjunction(toMergeChildren) => toMergeChildren.foldLeft(trees) {
				case (toTry, childToMerge) => merge(toTry, childToMerge)
			}
			case _ => aux(trees, Nil)
		}
	}
}

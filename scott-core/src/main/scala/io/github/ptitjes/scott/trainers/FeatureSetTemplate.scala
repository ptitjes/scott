package io.github.ptitjes.scott.trainers

import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.Features._
import io.github.ptitjes.scott.utils.States.SourceTracker

import scala.annotation.tailrec
import scala.collection._

case class FeatureTemplate[X](items: List[Extractor[X, _]])

object FeatureTemplate {

	def apply[X](items: Extractor[X, _]*): FeatureTemplate[X] = FeatureTemplate(items.toList)

	def apply[X](items: IndexedSeq[Extractor[X, _]]): FeatureTemplate[X] = FeatureTemplate(items.toList)
}

trait FeatureSetTemplate[X, Y <: X] {

	def name: String

	def features(order: Int): List[FeatureTemplate[X]]

	def token(i: Int) = EToken[X](i)

	def tag(i: Int) = EPreviousTag[X](i)

	def not(predicate: Predicate[X]) = PNot[X](predicate)

	implicit class RichTagSelector[X](e: EPreviousTag[X]) {

		def equalTo(tag: Int) = e.test(_ == tag)
	}

	implicit class RichSelector[X, T](e: Selector[X, T]) {

		def optionSelect[U](selector: T => Option[U]) = PFreeSelector(e, selector)

		def select[U](selector: T => U) = PFreeSelector[X, T, U](e, v => Some(selector(v)))

		def test(predicate: T => Boolean) = PFreePredicate(e, predicate)
	}

	def buildFeatures[T](breadth: Int, order: Int,
	                     corpus: Corpus[Y],
	                     f: BitSet => T,
	                     observableExtract: X => Int,
	                     tagExtract: Y => Int): (FeatureTree[X, T], FeatureTree[X, T], Map[Int, BitSet]) = {

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

				val (observable, tag) = (observableExtract(history.current), tagExtract(history.current))
				val previousTags = source.tags

				if (!dictionary.contains(observable))
					dictionary(observable) = BitSet(tag)
				else
					dictionary(observable) += tag

				templates.foreach { template =>
					val optionalCombination = template.items.map(_(history, previousTags))
					if (!optionalCombination.exists(_.isEmpty) &&
						!optionalCombination.contains(Some(false))) {

						val combination = optionalCombination.flatten
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

		def generateAll(filter: FeatureTemplate[X] => Boolean): FeatureTree[X, T] =
			FTConjunction(
				templates.filter(filter)
					.map(template => generateFrom(template, allData(template), allTags, f))
					.foldLeft(List[FeatureTree[X, T]]())(merge)
			)

		val wordOnlyFeatures = generateAll(isWordOnly)
		val otherFeatures = generateAll(!isWordOnly(_))
		(wordOnlyFeatures, otherFeatures, dictionary)
	}

	private def isWordOnly(template: FeatureTemplate[X]) =
		template.items.forall {
			case EPreviousTag(_) => false
			case _ => true
		}

	private def generateFrom[T](template: FeatureTemplate[X],
	                            data: Map[List[_], BitSet],
	                            allTags: BitSet,
	                            f: (BitSet) => T): FeatureTree[X, T] = {

		def aux(items: List[Extractor[X, _]], data: Map[List[_], BitSet]): (FeatureTree[X, T], BitSet) = {

			if (items.isEmpty) {
				val filterTags = data.foldLeft(BitSet()) { case (collected, (_, tags)) => collected ++ tags}

				(FTLeaf(f(filterTags), filterTags), filterTags)
			} else {
				val perPrevious = data.groupBy(_._1.head).mapValues(
					_.map { case (seq, tags) => (seq.tail, tags)}
				)
				val valueToTreeTags = perPrevious.map { case (tag, inner) => (tag, aux(items.tail, inner))}
				val valueToTree = valueToTreeTags.map { case (value, (tree, tags)) => value -> tree}
				val filterTags = valueToTreeTags.foldLeft(BitSet()) { case (collected, (_, (_, tags))) => collected ++ tags}

				items.head match {
					case p: Predicate[X] => (p.makeGuard(valueToTree(true), filterTags), filterTags)
					case s: Selector[X, _] => (s.makeDispatch(valueToTree, filterTags), filterTags)
					case _ => throw new UnsupportedOperationException("Unknown extractor: " + items.head.getClass)
				}
			}
		}

		val (resultTree, _) = aux(template.items, data)
		resultTree
	}

	private def merge[T](trees: List[FeatureTree[X, T]],
	                     toMerge: FeatureTree[X, T]): List[FeatureTree[X, T]] = {

		def tryToMergeTrees(left: FeatureTree[X, T], right: FeatureTree[X, T]): Option[FeatureTree[X, T]] =
			(left, right) match {
				case (FTDispatch(lExtractor, lChildren, lFilter), FTDispatch(rExtractor, rChildren, rFilter))
					if lExtractor == rExtractor =>
					Some(FTDispatch(lExtractor, mergeFeatureMaps(lChildren, rChildren), lFilter ++ rFilter))
				case (FTGuard(lPredicate, lChild), FTGuard(rPredicate, rChild))
					if lPredicate == rPredicate =>
					Some(FTGuard(lPredicate, mergeTrees(lChild, rChild)))
				case _ => None
			}

		def mergeFeatureMaps[K](left: Map[K, FeatureTree[X, T]], right: Map[K, FeatureTree[X, T]]): Map[K, FeatureTree[X, T]] =
			right.foldLeft(left) {
				case (result, (key, tree)) =>
					result + (key ->
						(if (result.contains(key)) mergeTrees(result(key), tree)
						else tree))
			}

		def mergeTrees(tree: FeatureTree[X, T], toMerge: FeatureTree[X, T]): FeatureTree[X, T] =
			tree match {
				case FTConjunction(lChildren) => FTConjunction(merge(lChildren.toList, toMerge))
				case _ =>
					val merged = merge(List(tree), toMerge)
					if (merged.size == 1) merged.head
					else FTConjunction(merged)
			}

		@tailrec def aux(yetToTry: List[FeatureTree[X, T]],
		                 tried: List[FeatureTree[X, T]]): List[FeatureTree[X, T]] =
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

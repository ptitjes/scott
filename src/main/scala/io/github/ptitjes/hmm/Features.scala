package io.github.ptitjes.hmm

import scala.collection._

object Features {

	import Corpora._

	sealed trait Extractor[T] extends (History => T)

	sealed trait Predicate extends Extractor[Boolean]

	sealed trait WordPredicate extends Predicate {

		def from: Extractor[String]

		def apply(h: History): Boolean = {
			val word = from(h)

			if (word == null) false
			else this match {
				case PContainsUppercase(_) => word.exists(_.isUpper)
				case PUppercaseOnly(_) => word.forall(_.isUpper)
				case PContainsNumber(_) => word.exists(_.isDigit)
				case PContains(_, v) => word.indexOf(v) != -1
			}
		}
	}

	case class PContainsUppercase(from: Extractor[String]) extends WordPredicate

	case class PUppercaseOnly(from: Extractor[String]) extends WordPredicate

	case class PContainsNumber(from: Extractor[String]) extends WordPredicate

	case class PContains(from: Extractor[String], value: Char) extends WordPredicate

	case class PTagEqual(from: Extractor[Int], value: Int) extends Predicate {

		def apply(h: History): Boolean = from(h) == value
	}

	case class PNot(from: Extractor[Boolean]) extends Predicate {

		def apply(h: History): Boolean = !from(h)
	}

	case class EPrefixChar(index: Int) extends Extractor[Char] {

		def apply(h: History): Char = {
			val word = h.word.string
			if (word.length > index) word.charAt(index) else 0.toChar
		}
	}

	case class ESuffixChar(index: Int) extends Extractor[Char] {

		def apply(h: History): Char = {
			val word = h.word.string
			if (word.length > index) word.charAt(word.length - index - 1) else 0.toChar
		}
	}

	case class EWordString(index: Int) extends Extractor[String] {

		def apply(h: History): String = {
			val word = h.wordAt(index)
			if (word != null) word.string else null
		}
	}

	case class EWordCode(index: Int) extends Extractor[Int] {

		def apply(h: History): Int = {
			val word = h.wordAt(index)
			if (word == null) -1 else word.code
		}
	}

	case class EPreviousTag(index: Int) extends Extractor[Int] {

		def apply(h: History): Int = h.previousTag(index)
	}

	case class Weights(tags: BitSet, values: Array[Double]) {

		def apply(key: Int) = values(key)

		def update(key: Int, value: Double) =
			if (!tags(key)) throw new ArrayIndexOutOfBoundsException(key)
			else values(key) = value

		def foreach[U](f: (Int, Double) => U): Unit =
			tags.foreach(t => f(t, values(t)))

		def map(f: Double => Double): Weights =
			new Weights(tags, values.map(f))
	}

	sealed trait FeatureTree[T] {

		def foreachMatching(h: History, tags: BitSet)(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreachMatching(h, tags)(f))
			case FTDispatchInt(extract, children, filter) =>
				if ((tags & filter).nonEmpty) {
					val key = extract(h)
					if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
				}
			case FTDispatchChar(extract, children, filter) =>
				if ((tags & filter).nonEmpty) {
					val key = extract(h)
					if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
				}
			case FTGuard(predicate, child) => if (predicate(h)) child.foreachMatching(h, tags)(f)
			case FTLeaf(weights, filter) => if ((tags & filter).nonEmpty) f(weights)
		}

		def foreach(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(f))
			case FTDispatchInt(extract, children, tags) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTDispatchChar(extract, children, tags) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTGuard(predicate, child) => child.foreach(f)
			case FTLeaf(weights, filter) => f(weights)
		}

		def map[U](f: T => U): FeatureTree[U] = this match {
			case FTConjunction(children) => FTConjunction(children.map(c => c.map(f)))
			case FTDispatchInt(extract, children, tags) =>
				FTDispatchInt(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTDispatchChar(extract, children, tags) =>
				FTDispatchChar(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTGuard(predicate, child) => FTGuard(predicate, child.map(f))
			case FTLeaf(weights, filter) => FTLeaf(f(weights), filter)
		}
	}

	case class FTConjunction[T](children: Seq[FeatureTree[T]]) extends FeatureTree[T]

	case class FTDispatchInt[T](extract: Extractor[Int],
	                            children: Map[Int, FeatureTree[T]],
	                            tags: BitSet) extends FeatureTree[T]

	case class FTDispatchChar[T](extract: Extractor[Char],
	                             children: Map[Char, FeatureTree[T]],
	                             tags: BitSet) extends FeatureTree[T]

	case class FTGuard[T](predicate: Extractor[Boolean], child: FeatureTree[T]) extends FeatureTree[T]

	case class FTLeaf[T](weights: T, tags: BitSet) extends FeatureTree[T]

	case class FeatureTemplate(items: List[Extractor[_]], isUnleashed: Boolean = false) {
		def unleash = FeatureTemplate(items, isUnleashed = true)
	}

	object FeatureTemplate {

		def apply(items: Extractor[_]*): FeatureTemplate = FeatureTemplate(items.toList)

		def apply(items: IndexedSeq[Extractor[_]]): FeatureTemplate = FeatureTemplate(items.toList)
	}

	trait FeatureSetTemplate {

		import scala.language.postfixOps

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
					}
				}
			}

			val (resultTree, _) = aux(template.items, data)
			resultTree
		}
	}

}

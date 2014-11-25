package io.github.ptitjes.hmm

import scala.collection._

object Features {

	import Corpora._

	sealed trait Extractor[T] extends (History => T)

	sealed trait Predicate[T] extends Extractor[Boolean]

	abstract class WordPredicate extends Predicate[Word] {

		def from: Extractor[Word]

		def apply(h: History): Boolean = {
			val word = from(h)

			if (word == null) false
			else this match {
				case PContainsUppercase() => word.string.exists(_.isUpper)
				case PUppercaseOnly() => word.string.forall(_.isUpper)
				case PContainsNumber() => word.string.exists(_.isDigit)
				case PContains(v) => word.string.indexOf(v) != -1
			}
		}
	}

	case class PContainsUppercase(from: Extractor[Word]) extends WordPredicate

	case class PUppercaseOnly(from: Extractor[Word]) extends WordPredicate

	case class PContainsNumber(from: Extractor[Word]) extends WordPredicate

	case class PContains(from: Extractor[Word], value: Char) extends WordPredicate

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

	case class EWordAt(index: Int) extends Extractor[Word] {

		def apply(h: History): Word = h.wordAt(index)
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

		def this(breadth: Int, tags: BitSet) = this(tags, Array.ofDim[Double](breadth))

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
			case FTDispatchChar(extract, children, filter) =>
				if ((tags & filter).nonEmpty) {
					val key = extract(h)
					if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
				}
			case FTDispatchInt(extract, children) =>
				val key = extract(h)
				if (children.contains(key)) children(key).foreachMatching(h, tags)(f)
			case FTGuard(predicate, child) => if (predicate(h)) child.foreachMatching(h, tags)(f)
			case FTLeaf(weights, filter) => if ((tags & filter).nonEmpty) f(weights)
		}

		def foreach(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(f))
			case FTDispatchChar(extract, children, tags) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTDispatchInt(extract, children) =>
				children.foreach({ case (k, c) => c.foreach(f)})
			case FTGuard(predicate, child) => child.foreach(f)
			case FTLeaf(weights, filter) => f(weights)
		}

		def map[U](f: T => U): FeatureTree[U] = this match {
			case FTConjunction(children) => FTConjunction(children.map(c => c.map(f)))
			case FTDispatchChar(extract, children, tags) =>
				FTDispatchChar(extract, children.map { case (k, c) => (k, c.map(f))}, tags)
			case FTDispatchInt(extract, children) =>
				FTDispatchInt(extract, children.map { case (k, c) => (k, c.map(f))})
			case FTGuard(predicate, child) => FTGuard(predicate, child.map(f))
			case FTLeaf(weights, filter) => FTLeaf(f(weights), filter)
		}
	}

	case class FTConjunction[T](children: Seq[FeatureTree[T]]) extends FeatureTree[T]

	case class FTDispatchChar[T](extract: Extractor[Char],
	                             children: Map[Char, FeatureTree[T]],
	                             tags: BitSet) extends FeatureTree[T]

	case class FTDispatchInt[T](extract: Extractor[Int], children: Map[Int, FeatureTree[T]]) extends FeatureTree[T]

	case class FTGuard[T](predicate: Predicate, child: FeatureTree[T]) extends FeatureTree[T]

	case class FTLeaf[T](weights: T, tags: BitSet) extends FeatureTree[T]

	trait FeatureSetTemplate {

		def features(order: Int): List[FeatureTemplate[_]]

		def buildFeatures[T](breadth: Int, order: Int,
		                     corpus: Corpus[Sequence with Annotation],
		                     f: BitSet => T): (FeatureTree[T], FeatureTree[T], Map[Int, BitSet]) = {

			val templates = features(order)
			val dictionaryCrawler = CrwlWordsAt(List(0))

			val crawlers = templates.map(_.crawler).toSet + dictionaryCrawler
			val dataStructures: Map[CorpusCrawler[_], _] =
				crawlers.map(c => (c, c.instantiateDataStructure())).toMap


			def getData[D](crawler: CorpusCrawler[D]): D = {
				dataStructures(crawler).asInstanceOf[D]
			}

			def feed[D](crawler: CorpusCrawler[D], tag: Int, history: History) {
				crawler.feed(tag, history, getData(crawler))
			}

			corpus.sequences.foreach { s: Sequence with Annotation =>
				val iterator = s.annotatedIterator(breadth, order)
				while (iterator.hasNext) {
					val (_, tag) = iterator.next()
					val h = iterator.history

					crawlers.foreach(crawler => feed(crawler, tag, h))
				}
			}

			def generate[D](template: FeatureTemplate[D]): FeatureTree[T] = {
				template.generateFrom(getData(template.crawler), f)
			}

			val wordOnlyFeatures = FTConjunction(
				templates.filter(_.isWordOnly).map(generate(_))
			)
			val otherFeatures = FTConjunction(
				templates.filter(!_.isWordOnly).map(generate(_))
			)
			val dictionary = getData(dictionaryCrawler).map { case (w, tags) => (w.head.code, tags)}
			(wordOnlyFeatures, otherFeatures, dictionary)
		}
	}

	trait CorpusCrawler[D] {

		def instantiateDataStructure(): D

		def feed(tag: Int, history: History, data: D): Unit
	}

	trait FeatureTemplate[D] {

		def isWordOnly: Boolean

		def crawler: CorpusCrawler[D]

		def generateFrom[T](data: D, f: BitSet => T): FeatureTree[T]
	}

	case object CrwlAllTags extends CorpusCrawler[mutable.BitSet] {

		override def instantiateDataStructure(): mutable.BitSet = {
			mutable.BitSet()
		}

		override def feed(tag: Int, history: History, data: mutable.BitSet): Unit = {
			if (!data.contains(tag)) data += tag
		}
	}

	case class CrwlTagsAt(indexes: List[Int]) extends CorpusCrawler[mutable.Map[List[Int], BitSet]] {

		override def instantiateDataStructure(): mutable.Map[List[Int], BitSet] = {
			mutable.Map[List[Int], BitSet]()
		}

		override def feed(tag: Int, history: History, data: mutable.Map[List[Int], BitSet]): Unit = {
			val previousTags = indexes.map(i => history.previousTag(i))
			if (!data.contains(previousTags))
				data(previousTags) = BitSet(tag)
			else
				data(previousTags) += tag
		}
	}

	case class CrwlWordsAt(indexes: List[Int]) extends CorpusCrawler[mutable.Map[List[Word], BitSet]] {

		override def instantiateDataStructure(): mutable.Map[List[Word], BitSet] = {
			mutable.Map[List[Word], BitSet]()
		}

		override def feed(tag: Int, history: History, data: mutable.Map[List[Word], BitSet]): Unit = {
			val words = indexes.map(i => history.wordAt(i))
			if (!words.contains(null)) {
				if (!data.contains(words))
					data(words) = BitSet(tag)
				else
					data(words) += tag
			}
		}
	}

	case class FeaTemNgram(depth: Int) extends FeatureTemplate[mutable.Map[List[Int], BitSet]] {

		def isWordOnly = false

		override val crawler = CrwlTagsAt((1 to depth).toList)

		override def generateFrom[T](data: mutable.Map[List[Int], BitSet], f: (BitSet) => T): FeatureTree[T] = {

			def aux(ngrams: Map[List[Int], BitSet], index: Int): FeatureTree[T] = {
				if (ngrams.size == 1) {
					val allTags = ngrams.foldLeft(BitSet()) { case (collected, (_, tags)) => collected ++ tags}
					FTLeaf(f(allTags), BitSet() ++ allTags)
				} else {
					val perPrevious = ngrams.groupBy(_._1.head).mapValues(
						_.map { case (seq, tags) => (seq.tail, tags)}
					)
					FTDispatchInt(
						EPreviousTag(index),
						perPrevious.map {
							case (tag, innerNgrams) => tag -> aux(innerNgrams, index + 1)
						}
					)
				}
			}

			aux(data, 1)
		}
	}

	case object FeaTemUppercases extends FeatureTemplate[mutable.BitSet] {

		def isWordOnly = true

		override def crawler: CorpusCrawler[mutable.BitSet] = CrwlAllTags

		override def generateFrom[T](data: mutable.BitSet, f: (BitSet) => T): FeatureTree[T] = {
			val allTags = data

			FTConjunction(
				FTGuard(PContainsUppercase(EWordAt(0)), FTLeaf(f(allTags), allTags)) ::
					FTGuard(PUppercaseOnly(EWordAt(0)), FTLeaf(f(allTags), allTags)) ::
					FTGuard(PContainsUppercase(EWordAt(0)),
						FTDispatchInt(EPreviousTag(1),
							allTags.map(i => (i, FTLeaf(f(allTags), allTags))).toMap
						)
					) ::
					Nil
			)
		}
	}

	case class FeaTemContains(char: Char) extends FeatureTemplate[mutable.BitSet] {

		def isWordOnly = true

		override def crawler: CorpusCrawler[mutable.BitSet] = CrwlAllTags

		override def generateFrom[T](data: mutable.BitSet, f: (BitSet) => T): FeatureTree[T] = {
			val allTags = data
			FTGuard(PContains(EWordAt(0), char), FTLeaf(f(allTags), allTags))
		}
	}

	case object FeaTemContainsNumber extends FeatureTemplate[mutable.BitSet] {

		def isWordOnly = true

		override def crawler: CorpusCrawler[mutable.BitSet] = CrwlAllTags

		override def generateFrom[T](data: mutable.BitSet, f: (BitSet) => T): FeatureTree[T] = {
			val allTags = data
			FTGuard(PContainsNumber(EWordAt(0)), FTLeaf(f(allTags), allTags))
		}
	}


	case class FeaTemWordAt(index: Int) extends FeatureTemplate[mutable.Map[List[Word], BitSet]] {

		def isWordOnly = true

		override val crawler: CorpusCrawler[mutable.Map[List[Word], BitSet]] = CrwlWordsAt(List(index))

		override def generateFrom[T](data: mutable.Map[List[Word], BitSet], f: BitSet => T): FeatureTree[T] = {
			FTDispatchInt(EWordCode(index),
				data.foldLeft(Map[Int, FeatureTree[T]]()) {
					case (m, (w, tags)) => m + (w.head.code -> FTLeaf(f(tags), BitSet() ++ tags))
				})
		}
	}

	case class FeaTemPrefix(maxSize: Int) extends FeaTemAffix {
		val extractor: Int => Extractor[Char] = i => EPrefixChar(i)
		val cruncher: String => (Char, String) = s => (s.charAt(0), s.substring(1))
	}

	case class FeaTemSuffix(maxSize: Int) extends FeaTemAffix {
		val extractor: Int => Extractor[Char] = i => ESuffixChar(i)
		val cruncher: String => (Char, String) = s => (s.last, s.substring(0, s.length - 1))
	}

	abstract class FeaTemAffix extends FeatureTemplate[mutable.Map[List[Word], BitSet]] {

		def isWordOnly = true

		override val crawler: CorpusCrawler[mutable.Map[List[Word], BitSet]] = CrwlWordsAt(List(0))

		override def generateFrom[T](data: mutable.Map[List[Word], BitSet], f: BitSet => T): FeatureTree[T] = {
			makeCharTree(data.map { case (w, tags) => (w.head.string, tags)}, maxSize,
				cruncher, extractor, tags => FTLeaf(f(tags), BitSet() ++ tags)
			)
		}

		def maxSize: Int

		def extractor: Int => Extractor[Char]

		def cruncher: String => (Char, String)

		def makeCharTree[T](words: Map[String, BitSet], maxLength: Int,
		                    cruncher: String => (Char, String),
		                    extractor: Int => Extractor[Char],
		                    leaf: BitSet => FeatureTree[T]): FeatureTree[T] = {

			val allTags = (0 until 15).toSet

			def aux(tree: Tree, index: Int): FeatureTree[T] = tree match {
				case Node(children, filter) =>
					val childrenFeatures = children.map {
						case (char, child) =>
							(char, aux(child, index + 1))
					}
					FTConjunction(
						leaf(filter) ::
							FTDispatchChar(extractor(index), childrenFeatures, BitSet() ++ filter) ::
							Nil
					)
				case Leaf(children, filter) => leaf(filter)
			}

			aux(makeTree(words, cruncher, maxLength), 0)
		}

		trait Tree

		case class Node(children: Map[Char, Tree], filter: BitSet) extends Tree

		case class Leaf(children: Map[String, BitSet], filter: BitSet) extends Tree

		def makeTree(words: Map[String, BitSet], cruncher: String => (Char, String), max: Int): Tree = {

			def crunch(l: Map[String, BitSet]): Map[Char, Map[String, BitSet]] = {
				l.toList.map {
					case (word, tags) =>
						val (head, rest) = cruncher(word)
						(head, (rest, tags))
				}.groupBy(_._1).mapValues(
				    _.map(_._2).groupBy(_._1).mapValues(
					    _.map(_._2).fold(BitSet())(_ ++ _)
				    )
					)
			}

			def doMakeTree(words: Map[String, BitSet], index: Int): (Tree, BitSet) = {
				if (index == max) {
					val allTags = words.foldLeft(BitSet()) {
						case (collected, (str, tags)) => collected ++ tags
					}
					(Leaf(words, allTags), allTags)
				} else {
					val crunched = crunch(words)
					val charToTreeAndTags = crunched.map {
						case (char, stringsToTags) =>
							val (tree, tags) = doMakeTree(stringsToTags - "", index + 1)

							val completeTags =
								if (stringsToTags.contains("")) tags ++ stringsToTags("")
								else tags

							(char, (tree, completeTags))
					}

					val allTags = charToTreeAndTags.foldLeft(BitSet()) {
						case (collected, (_, (_, tags))) => collected ++ tags
					}
					val charToTree = charToTreeAndTags.map {
						case (char, (tree, _)) => (char, tree)
					}
					(Node(charToTree, allTags), allTags)
				}
			}

			doMakeTree(words, 0)._1
		}
	}

}

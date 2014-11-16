package io.github.ptitjes.hmm

object Features {

	import Corpora._

	sealed trait Predicate extends (History => Boolean) {

		def apply(h: History): Boolean = this match {
			case PCapitalized() => h.word.string.charAt(0).isUpper
			case PNumber() => h.word.string.charAt(0).isDigit
			case PContains(v) => h.word.string.indexOf(v) != -1
			case PLength(i, l) => h.wordAt(i).string.length == l
		}
	}

	case class PCapitalized() extends Predicate

	case class PNumber() extends Predicate

	case class PContains(value: Char) extends Predicate

	case class PLength(index: Int, l: Int) extends Predicate

	sealed trait Extractor[T] extends (History => T)

	case class ECharAt(index: Int) extends Extractor[Char] {

		def apply(h: History): Char =
			if (h.word.string.length > index) h.word.string.charAt(index) else 0.toChar
	}

	case class EWordCode(index: Int) extends Extractor[Int] {

		def apply(h: History): Int = h.wordAt(index).code
	}

	case class EPreviousTag(index: Int) extends Extractor[Int] {

		def apply(h: History): Int = h.previousTags(index - 1)
	}

	sealed trait FeatureTree {
		def foreach(h: History)(f: Array[Double] => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(h)(f))
			case FTDispatch(extract, children) =>
				val key = extract(h)
				if (children.contains(key)) children(key).foreach(h)(f)
			case FTGuard(pred, child) => if (pred(h)) child.foreach(h)(f)
			case FTLeaf(weights) => f(weights)
			case FTNull =>
		}
	}

	case object FTNull extends FeatureTree

	case class FTConjunction(children: List[FeatureTree]) extends FeatureTree

	case class FTDispatch[T](extract: Extractor[T], children: Map[T, FeatureTree]) extends FeatureTree

	case class FTGuard(pred: Predicate, child: FeatureTree) extends FeatureTree

	case class FTLeaf(weights: Array[Double]) extends FeatureTree

	def makeBigramTree(breadth: Int, f: () => Array[Double]): FeatureTree =
		FTDispatch(
			EPreviousTag(1),
			(-1 until breadth).foldLeft(Map[Int, FeatureTree]())((m, i) => m + (i -> FTLeaf(f())))
		)

	def makeTrigramTree(breadth: Int, f: () => Array[Double]): FeatureTree =
		FTDispatch(
			EPreviousTag(2),
			(-1 until breadth).foldLeft(Map[Int, FeatureTree]())((m, i) => m + (i -> makeBigramTree(breadth, f)))
		)

	def makePrefixTree(prefixes: Set[String], f: () => Array[Double]): FeatureTree =
		makeCharTree(prefixes, 0, s => (s.charAt(0), s.substring(1)), i => FTLeaf(f()))

	def makeSuffixTree(suffixes: Set[String], f: () => Array[Double]): FeatureTree =
		makeCharTree(suffixes, 0, s => (s.last, s.substring(0, s.length - 1)), i => FTLeaf(f()))

	//	def makeWordTree(words: Set[String], f: () => Array[Double]): FeatureTree =
	//		makeCharTree(words, 0, s => (s.charAt(0), s.substring(1)),
	//			i => FTGuard(PLength(0, i), FTLeaf(f())))

	def makeWordTree(words: Set[Int], f: () => Array[Double]): FeatureTree =
		FTDispatch(EWordCode(0), words.foldLeft(Map[Int, FeatureTree]())((m, w) => m + (w -> FTLeaf(f()))))

	def makeCharTree(suffixes: Set[String],
	                 index: Int, ht: (String) => (Char, String),
	                 leaf: Int => FeatureTree): FeatureTree = {
		if (suffixes.contains("")) {
			if (suffixes.size == 1) {
				leaf(index)
			} else {
				FTConjunction(
					leaf(index) ::
						FTDispatch(
							ECharAt(index),
							crunchChars(suffixes - "", ht).map {
								case (char, strings) => (char, makeCharTree(strings, index + 1, ht, leaf))
							}
						) :: Nil
				)
			}
		} else {
			FTDispatch(
				ECharAt(index),
				crunchChars(suffixes, ht).map {
					case (char, strings) => (char, makeCharTree(strings, index + 1, ht, leaf))
				}
			)
		}
	}

	def crunchChars(strings: Set[String], f: String => (Char, String)): Map[Char, Set[String]] =
		strings.map(f).foldLeft(Map[Char, Set[String]]()) {
			case (map, (char, str)) =>
				map + (char -> (map.withDefaultValue(Set[String]())(char) + str))
		}
}

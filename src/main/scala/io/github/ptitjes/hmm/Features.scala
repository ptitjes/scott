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

		def apply(h: History): Int = {
			val word = h.wordAt(index)
			if (word == null) -1 else word.code
		}
	}

	case class EPreviousTag(index: Int) extends Extractor[Int] {

		def apply(h: History): Int = h.previousTags(index - 1)
	}

	sealed trait FeatureTree {
		def foreach(h: History)(f: Array[Double] => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(h)(f))
			case FTDispatchChar(extract, children) =>
				val key = extract(h)
				if (children.contains(key)) children(key).foreach(h)(f)
			case FTDispatchInt(extract, children) =>
				val key = extract(h)
				if (children.contains(key)) children(key).foreach(h)(f)
			case FTGuard(pred, child) => if (pred(h)) child.foreach(h)(f)
			case FTLeaf(weights) => f(weights)
			case FTNull =>
		}

		def addAveraged(other: FeatureTree, divider: Int): Unit = {
			def aux(d: FeatureTree, s: FeatureTree): Unit = (d, s) match {
				case (FTConjunction(children1), FTConjunction(children2)) =>
					children1.zip(children2).foreach { case (c1, c2) => aux(c1, c2)}
				case (FTDispatchChar(_, children1), FTDispatchChar(_, children2)) =>
					children1.zip(children2).foreach { case ((_, c1), (_, c2)) => aux(c1, c2)}
				case (FTDispatchInt(_, children1), FTDispatchInt(_, children2)) =>
					children1.zip(children2).foreach { case ((_, c1), (_, c2)) => aux(c1, c2)}
				case (FTGuard(_, c1), FTGuard(_, c2)) => aux(c1, c2)
				case (FTLeaf(w1), FTLeaf(w2)) =>
					for (i <- 0 until w1.length) w1(i) += w2(i) / divider
				case (FTNull, FTNull) =>
				case _ => throw new IllegalArgumentException()
			}
			aux(this, other)
		}
	}

	case object FTNull extends FeatureTree

	case class FTConjunction(children: Seq[FeatureTree]) extends FeatureTree

	case class FTDispatchChar(extract: Extractor[Char], children: Map[Char, FeatureTree]) extends FeatureTree

	case class FTDispatchInt(extract: Extractor[Int], children: Map[Int, FeatureTree]) extends FeatureTree

	case class FTGuard(pred: Predicate, child: FeatureTree) extends FeatureTree

	case class FTLeaf(weights: Array[Double]) extends FeatureTree

	def makeNgramTree(depth: Int, breadth: Int, f: () => Array[Double]): FeatureTree =
		if (depth == 0) FTLeaf(f())
		else
			FTDispatchInt(
				EPreviousTag(1),
				(-1 until breadth).foldLeft(Map[Int, FeatureTree]()) {
					(m, i) => m + (i -> makeNgramTree(depth - 1, breadth, f))
				}
			)

	def makePrefixTree(prefixes: Set[String], f: () => Array[Double]): FeatureTree =
		makeCharTree(prefixes, 0, s => (s.charAt(0), s.substring(1)), i => FTLeaf(f()))

	def makeSuffixTree(suffixes: Set[String], f: () => Array[Double]): FeatureTree =
		makeCharTree(suffixes, 0, s => (s.last, s.substring(0, s.length - 1)), i => FTLeaf(f()))

	def makeWordTree(index: Int, words: Set[Int], f: () => Array[Double]): FeatureTree =
		FTDispatchInt(EWordCode(index), words.foldLeft(Map[Int, FeatureTree]())((m, w) => m + (w -> FTLeaf(f()))))

	def makeCharTree(suffixes: Set[String],
	                 index: Int, ht: (String) => (Char, String),
	                 leaf: Int => FeatureTree): FeatureTree = {
		if (suffixes.contains("")) {
			if (suffixes.size == 1) {
				leaf(index)
			} else {
				FTConjunction(
					leaf(index) ::
						FTDispatchChar(
							ECharAt(index),
							crunchChars(suffixes - "", ht).map {
								case (char, strings) => (char, makeCharTree(strings, index + 1, ht, leaf))
							}
						) :: Nil
				)
			}
		} else {
			FTDispatchChar(
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

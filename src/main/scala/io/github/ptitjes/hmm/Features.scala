package io.github.ptitjes.hmm

object Features {

	import Corpora._

	sealed trait Predicate extends (History => Boolean) {

		def apply(h: History): Boolean = this match {
			case PUppercased() => h.word.string.exists(_.isUpper)
			case PNumber() => h.word.string.exists(_.isDigit)
			case PContains(v) => h.word.string.indexOf(v) != -1
			case PLength(i, l) => h.wordAt(i).string.length == l
		}
	}

	case class PUppercased() extends Predicate

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

	case class Weights(breadth: Int, tags: Set[Int], values: Array[Double]) {

		def this(breadth: Int, tags: Set[Int]) =
			this(breadth, tags, Array.ofDim(breadth))

		def apply(key: Int) = values(key)

		def update(key: Int, value: Double) = values(key) = value

		def foreach[U](f: (Int, Double) => U): Unit =
			tags.foreach(t => f(t, values(t)))

		def map(f: Double => Double): Weights =
			new Weights(breadth, tags, values.map(f))
	}

	sealed trait FeatureTree[T] {

		def foreachMatching(h: History)(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreachMatching(h)(f))
			case FTDispatchChar(extract, children) =>
				val key = extract(h)
				if (children.contains(key)) children(key).foreachMatching(h)(f)
			case FTDispatchInt(extract, children) =>
				val key = extract(h)
				if (children.contains(key)) children(key).foreachMatching(h)(f)
			case FTGuard(predicate, child) => if (predicate(h)) child.foreachMatching(h)(f)
			case FTLeaf(weights) => f(weights)
		}

		def foreach(f: T => Unit): Unit = this match {
			case FTConjunction(children) => children.foreach(c => c.foreach(f))
			case FTDispatchChar(extract, children) => children.foreach({ case (k, c) => c.foreach(f)})
			case FTDispatchInt(extract, children) => children.foreach({ case (k, c) => c.foreach(f)})
			case FTGuard(predicate, child) => child.foreach(f)
			case FTLeaf(weights) => f(weights)
		}

		def map[U](f: T => U): FeatureTree[U] = this match {
			case FTConjunction(children) => FTConjunction(children.map(c => c.map(f)))
			case FTDispatchChar(extract, children) => FTDispatchChar(extract, children.map { case (k, c) => (k, c.map(f))})
			case FTDispatchInt(extract, children) => FTDispatchInt(extract, children.map { case (k, c) => (k, c.map(f))})
			case FTGuard(predicate, child) => FTGuard(predicate, child.map(f))
			case FTLeaf(weights) => FTLeaf(f(weights))
		}

		//		def addAveraged(other: FeatureTree, divider: Int): Unit = {
		//			def aux(d: FeatureTree, s: FeatureTree): Unit = (d, s) match {
		//				case (FTConjunction(children1), FTConjunction(children2)) =>
		//					children1.zip(children2).foreach { case (c1, c2) => aux(c1, c2)}
		//				case (FTDispatchChar(_, children1), FTDispatchChar(_, children2)) =>
		//					children1.zip(children2).foreach { case ((_, c1), (_, c2)) => aux(c1, c2)}
		//				case (FTDispatchInt(_, children1), FTDispatchInt(_, children2)) =>
		//					children1.zip(children2).foreach { case ((_, c1), (_, c2)) => aux(c1, c2)}
		//				case (FTGuard(_, c1), FTGuard(_, c2)) => aux(c1, c2)
		//				case (FTLeaf(w1), FTLeaf(w2)) =>
		//					for (i <- 0 until w1.length) w1(i) += w2(i) / divider
		//				case _ => throw new IllegalArgumentException()
		//			}
		//			aux(this, other)
		//		}
	}

	case class FTConjunction[T](children: Seq[FeatureTree[T]]) extends FeatureTree[T]

	case class FTDispatchChar[T](extract: Extractor[Char], children: Map[Char, FeatureTree[T]]) extends FeatureTree[T]

	case class FTDispatchInt[T](extract: Extractor[Int], children: Map[Int, FeatureTree[T]]) extends FeatureTree[T]

	case class FTGuard[T](predicate: Predicate, child: FeatureTree[T]) extends FeatureTree[T]

	case class FTLeaf[T](weights: T) extends FeatureTree[T]

	def makeNgramTree[T](depth: Int, breadth: Int, f: Set[Int] => T): FeatureTree[T] =
		if (depth == 0) FTLeaf(f((0 until breadth).toSet))
		else
			FTDispatchInt(
				EPreviousTag(1),
				(-1 until breadth).foldLeft(Map[Int, FeatureTree[T]]()) {
					(m, i) => m + (i -> makeNgramTree(depth - 1, breadth, f))
				}
			)

	def makePrefixTree[T](prefixes: Map[String, Set[Int]], f: Set[Int] => T): FeatureTree[T] =
		makeCharTree(prefixes, 0, s => (s.charAt(0), s.substring(1)), tags => FTLeaf(f(tags)))

	def makeSuffixTree[T](suffixes: Map[String, Set[Int]], f: Set[Int] => T): FeatureTree[T] =
		makeCharTree(suffixes, 0, s => (s.last, s.substring(0, s.length - 1)), tags => FTLeaf(f(tags)))

	def makeWordTree[T](index: Int, words: Map[Int, Set[Int]], f: Set[Int] => T): FeatureTree[T] =
		FTDispatchInt(EWordCode(index),
			words.foldLeft(Map[Int, FeatureTree[T]]()) {
				case (m, (w, tags)) => m + (w -> FTLeaf(f(tags)))
			})

	def makeCharTree[T](affixes: Map[String, Set[Int]],
	                    index: Int, ht: (String) => (Char, String),
	                    leaf: Set[Int] => FeatureTree[T]): FeatureTree[T] = {
		if (affixes.contains("")) {
			if (affixes.size == 1) {
				leaf(affixes(""))
			} else {
				FTConjunction(
					leaf(affixes("")) ::
						FTDispatchChar(
							ECharAt(index),
							crunchChars(affixes - "", ht).map {
								case (char, strings) => (char, makeCharTree(strings, index + 1, ht, leaf))
							}
						) :: Nil
				)
			}
		} else {
			FTDispatchChar(
				ECharAt(index),
				crunchChars(affixes, ht).map {
					case (char, strings) => (char, makeCharTree(strings, index + 1, ht, leaf))
				}
			)
		}
	}

	def crunchChars(strings: Map[String, Set[Int]],
	                f: String => (Char, String)): Map[Char, Map[String, Set[Int]]] =
		strings.map { case (s, tags) => (f(s), tags)}
			.foldLeft(Map[Char, Map[String, Set[Int]]]()) {
			case (map, ((char, str), tags)) =>
				val previous = if (map.contains(char)) map(char) else Map[String, Set[Int]]()
				val newTags = if (previous.contains(str)) previous(str) ++ tags else tags
				map + (char -> (previous + (str -> newTags)))
		}
}

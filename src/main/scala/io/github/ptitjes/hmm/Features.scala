package io.github.ptitjes.hmm

object Features {

  case class WordComponents(word: String) {
    val length = word.length

    val c1 = word.substring(0, 1)

    val p1 = prefix(1)
    val p2 = prefix(2)
    val p3 = prefix(3)
    val p4 = prefix(4)

    val s1 = suffix(1)
    val s2 = suffix(2)
    val s3 = suffix(3)
    val s4 = suffix(4)

    def prefix(l: Int): String = {
      if (length >= l) word.substring(0, l).intern() else ""
    }

    def suffix(l: Int): String = {
      if (length >= l) word.substring(length - l).intern() else ""
    }
  }

  sealed trait WordPredicate extends (WordComponents => Boolean) {

    def apply(w: WordComponents): Boolean = this match {
      case WPEqual(v) => w.word == v
      case WPCapitalized() => w.c1.charAt(0).isUpper
      case WPNumber() => w.c1.charAt(0).isDigit
      case WPContains(v) => w.word.indexOf(v) != -1
      case WPSuffix1(v) => w.s1 eq v
      case WPSuffix2(v) => w.s2 eq v
      case WPSuffix3(v) => w.s3 eq v
      case WPSuffix4(v) => w.s4 eq v
      case WPPrefix1(v) => w.p1 eq v
      case WPPrefix2(v) => w.p2 eq v
      case WPPrefix3(v) => w.p3 eq v
      case WPPrefix4(v) => w.p4 eq v
    }
  }

  object WordPredicate {
    def makeWord(s: String) = WPEqual(s.intern())

    def makeSuffix(s: String) = s.length match {
      case 1 => WPSuffix1(s.intern())
      case 2 => WPSuffix2(s.intern())
      case 3 => WPSuffix3(s.intern())
      case 4 => WPSuffix4(s.intern())
    }

    def makePrefix(s: String) = s.length match {
      case 1 => WPPrefix1(s.intern())
      case 2 => WPPrefix2(s.intern())
      case 3 => WPPrefix3(s.intern())
      case 4 => WPPrefix4(s.intern())
    }
  }

  case class WPEqual(value: String) extends WordPredicate

  case class WPCapitalized() extends WordPredicate

  case class WPNumber() extends WordPredicate

  case class WPContains(value: Char) extends WordPredicate

  case class WPSuffix1(value: String) extends WordPredicate

  case class WPSuffix2(value: String) extends WordPredicate

  case class WPSuffix3(value: String) extends WordPredicate

  case class WPSuffix4(value: String) extends WordPredicate

  case class WPPrefix1(value: String) extends WordPredicate

  case class WPPrefix2(value: String) extends WordPredicate

  case class WPPrefix3(value: String) extends WordPredicate

  case class WPPrefix4(value: String) extends WordPredicate

  sealed trait Feature extends ((Int, Int, WordComponents) => Boolean) {
    def apply(h_2: Int, h_1: Int, w: WordComponents): Boolean = this match {
      case FTag1(t_1) => t_1 == h_1
      case FTag2(t_1, t_2) => t_2 == h_2 && t_1 == h_1
      case FWord(p) => p(w)
    }
  }

  case class FTag1(h_1: Int) extends Feature

  case class FTag2(h_1: Int, h_2: Int) extends Feature

  case class FWord(predicate: WordPredicate) extends Feature

  sealed trait FeatureTree {
    def foreach(h_2: Int, h_1: Int, w: WordComponents)(f: Array[Double] => Unit): Unit = this match {
      case FTConjunction(children) => children.foreach(c => c.foreach(h_2, h_1, w)(f))
      case FTDispatch(extract, children) =>
        val key = extract(h_2, h_1, w)
        if (children.contains(key)) children(key).foreach(h_2, h_1, w)(f)
      case FTGuard(pred, child) => if (pred(h_2, h_1, w)) child.foreach(h_2, h_1, w)(f)
      case FTLeaf(weights) => f(weights)
      case FTNull =>
    }
  }

  case object FTNull extends FeatureTree

  case class FTConjunction(children: List[FeatureTree]) extends FeatureTree

  case class FTDispatch[T](extract: (Int, Int, WordComponents) => T, children: Map[T, FeatureTree]) extends FeatureTree

  case class FTGuard(pred: (Int, Int, WordComponents) => Boolean, child: FeatureTree) extends FeatureTree

  case class FTLeaf(weights: Array[Double]) extends FeatureTree

  def makeBigramTree(breadth: Int, f: () => Array[Double]): FeatureTree =
    FTDispatch(
      (h_2, h_1, w) => h_1,
      (-1 until breadth).foldLeft(Map[Int, FeatureTree]())((m, i) => m + (i -> FTLeaf(f())))
    )

  def makeTrigramTree(breadth: Int, f: () => Array[Double]): FeatureTree =
    FTDispatch(
      (h_2, h_1, w) => h_2,
      (-1 until breadth).foldLeft(Map[Int, FeatureTree]())((m, i) => m + (i -> makeBigramTree(breadth, f)))
    )

  def makePrefixTree(prefixes: Set[String], f: () => Array[Double]): FeatureTree =
    makeCharTree(prefixes, 0, s => (s.charAt(0), s.substring(1)), i => FTLeaf(f()))

  def makeSuffixTree(suffixes: Set[String], f: () => Array[Double]): FeatureTree =
    makeCharTree(suffixes, 0, s => (s.last, s.substring(0, s.length - 1)), i => FTLeaf(f()))

  def makeWordTree(words: Set[String], f: () => Array[Double]): FeatureTree =
    makeCharTree(words, 0, s => (s.charAt(0), s.substring(1)),
      i => FTGuard((h_2, h_1, w) => w.length == i, FTLeaf(f())))

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
              (h_2, h_1, w) => if (w.length > index) w.word.charAt(index) else 0.toChar,
              crunchChars(suffixes - "", ht).map {
                case (char, strings) => (char, makeCharTree(strings, index + 1, ht, leaf))
              }
            ) :: Nil
        )
      }
    } else {
      FTDispatch(
        (h_2, h_1, w) => if (w.length > index) w.word.charAt(index) else 0.toChar,
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

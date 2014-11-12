package io.github.ptitjes.hmm

object Features {

	case class WordComponents(word: String) {
		val length = word.length
		val p1 = word.substring(0, 1)
		val s1 = suffix(1)
		val s2 = suffix(2)
		val s3 = suffix(3)
		val s4 = suffix(4)
		val s5 = suffix(5)

		def suffix(l: Int): String = {
			if (length >= l) word.substring(length - l).intern() else ""
		}
	}

	sealed trait WordPredicate extends (WordComponents => Boolean) {

		def apply(w: WordComponents): Boolean = this match {
			case WPCapitalized() => w.p1.charAt(0).isUpper
			case WPNumber() => w.p1.charAt(0).isDigit
			case WPContains(v) => w.word.indexOf(v) != -1
			case WPSuffix1(v) => w.s1 eq v
			case WPSuffix2(v) => w.s2 eq v
			case WPSuffix3(v) => w.s3 eq v
			case WPSuffix4(v) => w.s4 eq v
			case WPSuffix5(v) => w.s5 eq v
		}
	}

	object WordPredicate {
		def makeSuffix(s: String) = s.length match {
			case 1 => WPSuffix1(s.intern())
			case 2 => WPSuffix2(s.intern())
			case 3 => WPSuffix3(s.intern())
			case 4 => WPSuffix4(s.intern())
			case 5 => WPSuffix5(s.intern())
		}
	}

	case class WPCapitalized() extends WordPredicate

	case class WPNumber() extends WordPredicate

	case class WPContains(value: Char) extends WordPredicate

	case class WPSuffix1(value: String) extends WordPredicate

	case class WPSuffix2(value: String) extends WordPredicate

	case class WPSuffix3(value: String) extends WordPredicate

	case class WPSuffix4(value: String) extends WordPredicate

	case class WPSuffix5(value: String) extends WordPredicate

	sealed trait Feature extends ((Int, Int, WordComponents) => Boolean) {
		def apply(h_2: Int, h_1: Int, w: WordComponents): Boolean = this match {
			case FH0(p) => p(w)
			case FH1(p, t_1) => t_1 == h_1 && p(w)
			case FH2(p, t_1, t_2) => t_2 == h_2 && t_1 == h_1 && p(w)
		}
	}

	case class FH0(predicate: WordPredicate) extends Feature

	case class FH1(predicate: WordPredicate, h_1: Int) extends Feature

	case class FH2(predicate: WordPredicate, h_1: Int, h_2: Int) extends Feature

}

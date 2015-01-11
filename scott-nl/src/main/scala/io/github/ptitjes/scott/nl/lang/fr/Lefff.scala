package io.github.ptitjes.scott.nl.lang.fr

import java.nio.charset.Charset
import java.util.Locale

import io.github.ptitjes.scott.api.TagSet
import io.github.ptitjes.scott.utils.Trie

import scala.collection.{Set, BitSet, mutable}
import scala.io.Source

/**
 * @author Didier Villevalois
 */
object Lefff {

	def parse(path: String, version: String): Trie[BitSet] =
		parse(Source.fromFile(path + "/lefff-ext-" + version + ".txt")(Charset forName "ISO-8859-15"))

	def parse(source: Source): Trie[BitSet] = {
		val elements = mutable.HashMap[String, mutable.Set[Int]]()
		source.getLines().foreach {
			case l if !l.isEmpty =>
				val split = l.split('\t')
				val word = split(0).toLowerCase(Locale.FRENCH)
				val tag = LefffTags(split(2))
				if (elements.contains(word)) {
					elements(word) += tag
				} else {
					elements(word) = mutable.Set(tag)
				}
			case _ =>
		}
		Trie[BitSet]() ++ elements.mapValues(BitSet() ++ _)
	}

	object LefffTags extends TagSet(
		IndexedSeq(
			"adj",
			"adjPref",
			"adv",
			"advneg",
			"advPref",
			"advm",
			"advp",
			"caimp",
			"ce",
			"cf",
			"csu",
			"coo",
			"cla",
			"cld",
			"clg",
			"cll",
			"cln",
			"clneg",
			"clr",
			"clar",
			"cldr",
			"det",
			"cfi",
			"etr",
			"ilimp",
			"meta",
			"nc",
			"np",
			"prel",
			"prep",
			"pres",
			"pri",
			"ponctw",
			"poncts",
			"parento",
			"parentf",
			"que",
			"que_restr",
			"sbound",
			"suffAdj",
			"epsilon",
			"pro",
			"v",
			"auxAvoir",
			"auxEtre",
			":GA",
			":GN",
			":GP",
			":GR",
			":NV",
			":PV",
			"GA:",
			"GN:",
			"GP:",
			"GR:",
			"NV:",
			"PV:"
		)
	)

}

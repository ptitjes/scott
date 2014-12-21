package io.github.ptitjes.scott

import java.io._

import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott.Features._
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization._

import scala.collection._
import scala.reflect.ClassTag

sealed trait HiddenMarkovModel {

	def breadth: Int

	def depth: Int

	def isUnknown(o: Word): Boolean
}

case class HMMGenerative(breadth: Int, depth: Int,
                         T: Array[Array[Array[Double]]],
                         E: Map[Int, Array[Double]],
                         UE: Array[Double],
                         dictionary: Map[Int, BitSet]) extends HiddenMarkovModel {

	def isUnknown(o: Word): Boolean = !E.contains(o.code)
}

case class HMMDiscriminant(breadth: Int, depth: Int,
                           wordOnlyFeatures: FeatureTree[Weights],
                           otherFeatures: FeatureTree[Weights],
                           dictionary: Map[Int, BitSet]) extends HiddenMarkovModel {

	def isUnknown(o: Word): Boolean = !dictionary.contains(o.code)
}

object HiddenMarkovModel {

	def initializeMatrixTree[T: ClassTag](breadth: Int, depth: Int) = {
		val array: Array[Array[Array[T]]] = Array.ofDim(depth + 1)
		for (i <- 0 to depth) {
			val sourceStateCount = pow(breadth, i)
			array(i) = Array.ofDim[T](breadth, sourceStateCount)
		}
		array
	}

	implicit val formats = Serialization.formats(
		ShortTypeHints(List(
			classOf[HMMGenerative], classOf[HMMDiscriminant],

			classOf[PContainsUppercase], classOf[PUppercaseOnly],
			classOf[PContainsNumber], classOf[PContains],
			classOf[PTagEqual], classOf[PNot],

			classOf[EPrefixChar], classOf[ESuffixChar],
			classOf[EWordCode], classOf[EWordString], classOf[EPreviousTag],

			classOf[FTConjunction[Array[Double]]], classOf[FTGuard[Array[Double]]],
			classOf[FTDispatchInt[Array[Double]]], classOf[FTDispatchChar[Array[Double]]],
			classOf[FTLeaf[Array[Double]]]
		))) +
		new CharKeySerializer + new CharacterKeySerializer +
		new CharSerializer + new CharacterSerializer + new BitSetSerializer + new WeightsSerializer

	def fromFile(file: File): HiddenMarkovModel = {
		using(new FileReader(file)) {
			fileInput => using(new BufferedReader(fileInput)) {
				in => read[HiddenMarkovModel](in)
			}
		}
	}

	def toFile(hmm: HiddenMarkovModel, file: File): Unit = {
		if (!file.getParentFile.exists()) file.getParentFile.mkdirs()

		using(new FileWriter(file)) {
			fileOutput => using(new PrintWriter(fileOutput)) {
				out => write(hmm, out)
			}
		}
	}

	class CharKeySerializer extends CustomKeySerializer[Char](format => ( {
		case key => key.charAt(0)
	}, {
		case key: Char => key.toString
	}
		))

	class CharacterKeySerializer extends CustomKeySerializer[Object](format => ( {
		case key => new Character(key.charAt(0))
	}, {
		case key: Character => key.toString
	}
		))

	class CharSerializer extends CustomSerializer[Char](format => ( {
		case JString(v) => v.charAt(0)
	}, {
		case v: Char => JString(v.toString)
	}
		))

	class CharacterSerializer extends CustomSerializer[Object](format => ( {
		case JString(v) => new Character(v.charAt(0))
	}, {
		case v: Character => JString(v.toString)
	}
		))

	class BitSetSerializer extends CustomSerializer[BitSet](format => ( {
		case a: JArray => immutable.BitSet.fromBitMask(Extraction.extract[Array[Long]](a))
	}, {
		case v: BitSet => Extraction.decompose(v.toBitMask)
	}
		))

	class WeightsSerializer extends CustomSerializer[Weights](format => ( {
		case v: JValue =>
			val map = Extraction.extract[Map[Int, Double]](v)
			val maxIndex = map.foldLeft(0) { case (m, (i, _)) => if (i > m) i else m}
			Weights(BitSet() ++ map.keySet, map.foldLeft(Array.ofDim[Double](maxIndex + 1)) {
				case (weights, (tag, weight)) =>
					weights(tag) = weight
					weights
			})
	}, {
		case Weights(tags, weights) => Extraction.decompose(
			tags.map(tag => (tag, weights(tag))).toMap
		)
	}
		))

}

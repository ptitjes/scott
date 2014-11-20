package io.github.ptitjes.hmm

import java.io._

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.Features._
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
			classOf[PUppercased], classOf[PNumber], classOf[PContains], classOf[PLength],
			classOf[ECharAt], classOf[EWordCode], classOf[EPreviousTag],
			classOf[FTConjunction[Array[Double]]], classOf[FTDispatchInt[Array[Double]]],
			classOf[FTDispatchChar[Array[Double]]], classOf[FTGuard[Array[Double]]],
			classOf[FTLeaf[Array[Double]]]
		))) +
		new CharKeySerializer + new CharacterKeySerializer +
		new CharSerializer + new CharacterSerializer

	def fromFile(file: File): HiddenMarkovModel = {
		using(new FileReader(file)) {
			fileReader => using(new BufferedReader(fileReader)) {
				reader => read[HiddenMarkovModel](reader)
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

}

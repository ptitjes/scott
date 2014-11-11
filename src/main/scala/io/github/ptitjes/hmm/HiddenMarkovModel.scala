package io.github.ptitjes.hmm

import java.io._

import io.github.ptitjes.hmm.Utils._
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization._

import scala.collection._
import scala.reflect.ClassTag

case class HiddenMarkovModel(breadth: Int, depth: Int,
                             t: MatrixTree[Double],
                             e: Map[Int, Array[Double]], ue: Array[Double]) {

	def T(d: Int): Array[Array[Double]] = t(d)

	def E(o: Int): Array[Double] =
		if (e.contains(o)) e(o) else ue

	def isUnknown(o: Int): Boolean = !e.contains(o)
}

case class MatrixTree[T: ClassTag](breadth: Int, depth: Int, tree: Array[Array[Array[T]]]) {

	def apply(d: Int): Array[Array[T]] = tree(d)
}

object MatrixTree {

	def apply[T: ClassTag](breadth: Int, depth: Int): MatrixTree[T] =
		new MatrixTree(breadth, depth, MatrixTree.initializeTree(breadth, depth))

	def initializeTree[T: ClassTag](breadth: Int, depth: Int) = {
		val array: Array[Array[Array[T]]] = Array.ofDim(depth + 1)
		for (i <- 0 to depth) {
			val targetStateCount = pow(breadth, i)
			array(i) = Array.ofDim[T](breadth, targetStateCount)
		}
		array
	}
}

object HiddenMarkovModel {

	implicit val formats = Serialization.formats(NoTypeHints) +
		new HMMSerializer

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

	class HMMSerializer extends CustomSerializer[HiddenMarkovModel](format => ( {
		case JObject(JField("b", JInt(b)) :: JField("d", JInt(d)) ::
			JField("t", t) :: JField("e", e) :: JField("ue", ue) :: Nil) =>

			val breadth = b.toInt
			val depth = d.toInt
			HiddenMarkovModel(breadth, depth,
				MatrixTree(breadth, depth, Extraction.extract[Array[Array[Array[Double]]]](t)),
				Extraction.extract[Map[Int, Array[Double]]](e),
				Extraction.extract[Array[Double]](ue))
	}, {
		case hmm: HiddenMarkovModel =>
			JObject(JField("b", JInt(hmm.breadth)) ::
				JField("d", JInt(hmm.depth)) ::
				JField("t", Extraction.decompose(hmm.t.tree)) ::
				JField("e", Extraction.decompose(hmm.e)) ::
				JField("ue", Extraction.decompose(hmm.ue)) :: Nil)
	}
		))

}

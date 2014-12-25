package io.github.ptitjes.scott

import java.io._

import io.github.ptitjes.scott.Features._
import io.github.ptitjes.scott.Utils._

import scala.collection._
import scala.reflect.ClassTag

//import scala.pickling._
//import scala.pickling.json._

sealed trait HiddenMarkovModel[X, Y] extends Serializable {

	def breadth: Int

	def depth: Int

	def isUnknown(o: Int): Boolean

	def dictionary: Map[Int, BitSet]

	def observableExtract: X => Int

	def builder: (X, Int) => Y
}

case class HMMGenerative[X, Y](breadth: Int, depth: Int,
                               T: Array[Array[Array[Double]]],
                               E: Map[Int, Array[Double]],
                               UE: Array[Double],
                               dictionary: Map[Int, BitSet],
                               observableExtract: X => Int,
                               builder: (X, Int) => Y) extends HiddenMarkovModel[X, Y] {

	def isUnknown(o: Int): Boolean = !E.contains(o)
}

case class HMMDiscriminant[X, Y](breadth: Int, depth: Int,
                                 wordOnlyFeatures: FeatureTree[X, Weights],
                                 otherFeatures: FeatureTree[X, Weights],
                                 dictionary: Map[Int, BitSet],
                                 observableExtract: X => Int,
                                 builder: (X, Int) => Y) extends HiddenMarkovModel[X, Y] {

	def isUnknown(o: Int): Boolean = !dictionary.contains(o)
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

	def readFrom[X, Y](file: File): HiddenMarkovModel[X, Y] = {
		using(new FileInputStream(file)) {
			fIn => using(new ObjectInputStream(fIn)) {
				in => in.readObject().asInstanceOf[HiddenMarkovModel[X, Y]]
			}
		}
		//		using(new FileInputStream(file)) {
		//			in => new BinaryPickleStream(in).unpickle[HiddenMarkovModel[X, Y]]
		//		}
	}

	def writeTo[X, Y](hmm: HiddenMarkovModel[X, Y], file: File): Unit = {
		if (!file.getParentFile.exists()) file.getParentFile.mkdirs()

		using(new FileOutputStream(file)) {
			fOut => using(new ObjectOutputStream(fOut)) {
				out => out.writeObject(hmm)
			}
		}
		//		using(new FileOutputStream(file)) {
		//			out => hmm.pickleTo(new OutputStreamOutput(out))
		//		}
	}
}

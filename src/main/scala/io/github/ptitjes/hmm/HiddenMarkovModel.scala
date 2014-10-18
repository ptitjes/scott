package io.github.ptitjes.hmm

import java.io.{PrintWriter, FileWriter, File}

import scala.io.Source
import scala.reflect.ClassTag

case class HiddenMarkovModel(breadth: Int, depth: Int,
                             pi: MatrixTree[Double], t: Array[Array[Double]],
                             e: Map[Int, Array[Double]], ue: Array[Double]) {

  def PI: MatrixTree[Double] = pi

  def T: Array[Array[Double]] = t

  def E(o: Int): Array[Double] =
    if (e.contains(o)) e(o) else ue
}

object HiddenMarkovModel {

  /*def fromFile(file: File): HiddenMarkovModel = {
    val lines = Source.fromFile(file).getLines()

    val nbe = lines.next().toInt
    val nbo = lines.next().toInt

    val hmm = HiddenMarkovModel(nbe, nbo)
    for (i <- 0 to nbe - 1)
      hmm.PI(i) = Math.log(lines.next().toDouble)

    for (i <- 0 to nbe - 1; j <- 0 to nbe - 1)
      hmm.T(i)(j) = Math.log(lines.next().toDouble)

    for (i <- 0 to nbe - 1; j <- 0 to nbo - 1)
      hmm.E(i)(j) = Math.log(lines.next().toDouble)

    hmm
  }

  def toFile(hmm: HiddenMarkovModel, file: File): Unit = {
    using(new FileWriter(file, true)) {
      fileWriter => using(new PrintWriter(fileWriter)) {
        out =>
          out.println(hmm.nbe)
          out.println(hmm.nbo)

          for (i <- 0 to hmm.nbe - 1)
            out.println(Math.exp(hmm.PI(i)))

          for (i <- 0 to hmm.nbe - 1; j <- 0 to hmm.nbe - 1)
            out.println(Math.exp(hmm.T(i)(j)))

          for (i <- 0 to hmm.nbe - 1; j <- 0 to hmm.nbo - 1)
            out.println(Math.exp(hmm.E(i)(j)))
      }
    }
  }*/

  def using[A <: AutoCloseable, B](param: A)(f: A => B): B =
    try {
      f(param)
    } finally {
      param.close()
    }
}

case class MatrixTree[T: ClassTag](breadth: Int, depth: Int) {

  private val tree = {
    val array: Array[Element] = Array.ofDim(depth)
    for (i <- 0 until depth) {
      array(i) = Element(i)
    }
    array
  }

  def apply(d: Int): Element = tree(d)

  case class Element(depth: Int) {

    private val rows = math.pow(breadth, depth).asInstanceOf[Int]
    private val cols = breadth
    private val probabilities: Array[Array[T]] = Array.ofDim[T](cols, rows)

    def apply(j: Int): Array[T] = probabilities(j)
  }

}
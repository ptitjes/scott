package io.github.ptitjes.hmm

import java.io.{PrintWriter, FileWriter, File}

import scala.io.Source
import scala.reflect.ClassTag

case class HiddenMarkovModel(breadth: Int, depth: Int,
                             t: MatrixTree[Double],
                             e: Map[Int, Array[Double]], ue: Array[Double]) {

  def T(d: Int): Array[Array[Double]] = t(d)

  def E(o: Int): Array[Double] =
    if (e.contains(o)) e(o) else ue

  def isUnknown(o: Int): Boolean = !e.contains(o)
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

  import Utils._

  private val tree = {
    val array: Array[Array[Array[T]]] = Array.ofDim(depth + 1)
    for (i <- 0 to depth) {
      val targetStateCount = pow(breadth, depth)
      array(i) = Array.ofDim[T](breadth, targetStateCount)
    }
    array
  }

  def apply(d: Int): Array[Array[T]] = tree(d)
}
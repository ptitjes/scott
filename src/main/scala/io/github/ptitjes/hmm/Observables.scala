package io.github.ptitjes.hmm

import java.io.File

import scala.io.Source

object Observables {

  def fromFile(file: File): Iterator[Int] = {
    Source.fromFile(file).getLines().map(s => s.toInt)
  }
}

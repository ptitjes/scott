package io.github.ptitjes.hmm

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Corpora {

  case class Corpus[+T <: Sequence](sequences: Seq[T]) {

    def size: Int = sequences.size

    def slice(from: Int, until: Int): Corpus[T] =
      Corpus(sequences.slice(from, until))

    def splitBy(ratio: Double): (Corpus[T], Corpus[T]) = {
      val splitIndex = (size * ratio).toInt
      (slice(0, splitIndex), slice(splitIndex + 1, size))
    }

    def map[U <: Sequence](f: T => U): Corpus[U] =
      Corpus(sequences.map(f))
  }

  trait Sequence {
    def observables: Array[Int]
  }

  trait Annotation {
    self: Sequence =>
    def states: Array[Int]

    def observablesAndStates: Array[(Int, Int)] = observables.zip(states)
  }

  case class AnnotatedSequence(observables: Array[Int], states: Array[Int])
    extends Sequence with Annotation

  def fromSource(source: Source): Corpus[Sequence with Annotation] = {
    val sequences = scala.collection.mutable.ListBuffer[Sequence with Annotation]()

    val observables: ArrayBuffer[Int] = ArrayBuffer()
    val states: ArrayBuffer[Int] = ArrayBuffer()
    source.getLines().foreach { s =>
      if (s.isEmpty) {
        sequences += AnnotatedSequence(observables.toArray, states.toArray)
        observables.clear()
        states.clear()
      }
      else {
        val split = s.split(' ')
        observables += split(0).toInt
        states += split(1).toInt
      }
    }

    Corpus(sequences)
  }

  def fromFile(file: File): Corpus[Sequence with Annotation] = fromSource(Source.fromFile(file))

  def fromURL(url: java.net.URL): Corpus[Sequence with Annotation] = fromSource(Source.fromURL(url))
}

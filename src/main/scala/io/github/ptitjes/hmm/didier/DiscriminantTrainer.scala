package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.didier.EmittingTraining.UNKNOWN_THRESHOLD

import scala.collection.mutable

object DiscriminantTrainer extends Algorithm[Trainer] {

	def name: String = "Disc"

	override def parameters: Set[Parameter[_]] = Set(ORDER, ITERATION_COUNT)

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer {

		import io.github.ptitjes.hmm.Corpora._

		def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
			val breadth = stateCount(corpus)
			val depth = configuration(ORDER)

			val size = pow(breadth, depth)

			val T = MatrixTree[Double](breadth, depth)
			var E: mutable.Map[Int, Array[Double]] = mutable.Map()
			val UE: Array[Double] = Array.ofDim(breadth)

			corpus.sequences.foreach { s: Sequence with Annotation =>
				s.observablesAndStates.foreach { case (word, cat) =>
					if (!E.contains(word)) {
						E += word -> Array.ofDim(breadth)
					}
				}
			}

			val hmm = HiddenMarkovModel(breadth, depth, T, E, UE)
			val decoder = FullDecoder.instantiate(configuration)
			decoder.setHmm(hmm)

			val iterationCount = configuration(ITERATION_COUNT)
			for (i <- 1 to iterationCount) {
				corpus.sequences.foreach { refSeq: Sequence with Annotation =>

					val hypSeq = decoder.decode(refSeq)

					if (refSeq.observables.length != hypSeq.observables.length || refSeq.states.length != hypSeq.states.length) {
						throw new IllegalStateException("Observable length mismatch!")
					}

					var d = 0
					var previousHypState = 0
					var previousRefState = 0

					refSeq.observablesAndStates.zip(hypSeq.observablesAndStates).foreach {
						case ((oRef, sRef), (oHyp, sHyp)) =>
							if (oRef != oHyp) {
								throw new IllegalStateException("Observable mismatch!")
							}

							val Eo = E(oRef)
							Eo(sRef) += 1
							Eo(sHyp) -= 1

							val Td = T(d)
							Td(sRef)(previousRefState) += 1
							Td(sHyp)(previousHypState) -= 1

							if (d < depth) {
								d += 1
							}
							previousRefState = previousRefState * breadth + sRef
							previousRefState = previousRefState % size
							previousHypState = previousHypState * breadth + sHyp
							previousHypState = previousHypState % size
					}
				}
			}

			for (j <- 0 until breadth) {
				UE(j) = log(1) - log(breadth)
			}

			hmm
		}
	}

	def indicator(test: Boolean) = if (test) 1 else 0
}

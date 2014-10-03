import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.didier._

val h0 = HiddenMarkovModel(2, 3)
h0.PI(0) = 0.5
h0.PI(1) = 0.5
h0.T(0)(0) = 0.6
h0.T(0)(1) = 0.4
h0.T(1)(0) = 0.4
h0.T(1)(1) = 0.6
h0.E(0)(0) = 0.8
h0.E(0)(1) = 0
h0.E(0)(2) = 0.2
h0.E(1)(0) = 0
h0.E(1)(1) = 0.8
h0.E(1)(2) = 0.2

val o1 = Seq(0, 1, 1, 2, 0, 1, 2, 1)
val mpss1 = Viterbi.mostProbableStateSequence(o1.iterator, h0).toSeq

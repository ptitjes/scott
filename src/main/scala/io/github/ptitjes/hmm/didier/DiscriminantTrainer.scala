package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Features._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

import scala.collection.mutable

object DiscriminantTrainer extends Algorithm[Trainer] {

	def name: String = "Disc"

	override def parameters: Set[Parameter[_]] = Set(ORDER, ITERATION_COUNT, FEATURES, AVERAGING)

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	object FEATURES extends BooleanParameter("Features", true)

	object AVERAGING extends BooleanParameter("Averaging", true)

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer {

		import io.github.ptitjes.hmm.Corpora._

		def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
			val breadth = stateCount(corpus)
			val depth = configuration(ORDER)
			val useFeatures = configuration(FEATURES)
			val useAveraging = configuration(AVERAGING)

			val size = pow(breadth, depth)

			val T = MatrixTree[Double](breadth, depth)
			var E: mutable.Map[Int, Array[Double]] = mutable.Map()

			corpus.sequences.foreach { s: Sequence with Annotation =>
				s.observablesAndStates.foreach { case (word, cat) =>
					if (!E.contains(word)) {
						E += word -> Array.ofDim(breadth)
					}
				}
			}

			val featuresWeight =
				if (!useFeatures) null
				else {
					val features =
						List[Feature](FH0(WPContains('-')), FH0(WPNumber())) ++
							(for {
								h_1 <- -1 until breadth
							} yield FH1(WPCapitalized(), h_1)) ++ /*
							(for {
								s <- SUFFIXES
								p = WordPredicate.makeSuffix(s)
							} yield FH0(p)) ++
							(for {
								h_1 <- -1 until breadth
								s <- SUFFIXES
								p = WordPredicate.makeSuffix(s)
							} yield FH1(p, h_1)) ++*/
							(for {
								h_1 <- -1 until breadth
								h_2 <- -1 until breadth
								s <- SUFFIXES
								p = WordPredicate.makeSuffix(s)
							} yield FH2(p, h_1, h_2))

					features.map(f => (f, Array.ofDim[Double](breadth)))
				}

			val featureCount = if (!useFeatures) -1 else featuresWeight.size
			val featureInc = 1.0 / featureCount
			val emittingInc = 2.5

			val hmm =
				if (!useFeatures)
					HiddenMarkovModel(breadth, depth, T, E, UEPShared(Array.fill(breadth)(avoidInfinity(-log(breadth)))))
				else
					HiddenMarkovModel(breadth, depth, T, E, UEPFeatureBased(featuresWeight))

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
					var Td = T(d)
					var previousHypState = 0
					var previousRefState = 0

					refSeq.observablesAndStates.zip(hypSeq.observablesAndStates).foreach {
						case ((oRef, sRef), (oHyp, sHyp)) =>
							if (oRef != oHyp) {
								throw new IllegalStateException("Observable mismatch!")
							}

							if (sRef != sHyp) {
								val Eo = E(oRef)
								Eo(sRef) += emittingInc
								Eo(sHyp) -= emittingInc
							}

							if (sRef != sHyp || previousRefState != previousHypState) {
								Td(sRef)(previousRefState) += 1
								Td(sHyp)(previousHypState) -= 1

								if (useFeatures) {
									val h_1_ref = if (d == 0) -1 else previousRefState % breadth
									val h_1_hyp = if (d == 0) -1 else previousHypState % breadth
									val h_2_ref = if (d <= 1) -1 else previousRefState / breadth % breadth
									val h_2_hyp = if (d <= 1) -1 else previousHypState / breadth % breadth
									val word = WordComponents(Lexica.WORDS(oRef))

									featuresWeight.foreach {
										case (f, weights) =>
											if (f(h_2_ref, h_1_ref, word)) {
												weights(sRef) += featureInc
											}
											if (f(h_2_hyp, h_1_hyp, word)) {
												weights(sHyp) -= featureInc
											}
									}
								}
							}

							if (d < depth) {
								d += 1
								Td = T(d)
							}
							previousRefState = (previousRefState * breadth + sRef) % size
							previousHypState = (previousHypState * breadth + sHyp) % size
					}
				}
			}

			hmm
		}
	}

	val SUFFIXES = List("'",
		"er",
		"ir",
		"re",
		"ie",
		"is",
		"es",
		"aux",
		"ion",
		"ions",
		"eur",
		"eurs",
		"euse",
		"euses",
		"se",
		"ses",
		"aire",
		"aires",
		"té",
		"é",
		"és",
		"ée",
		"ées",
		"iste",
		"istes",
		"isme",
		"ue",
		"ant",
		"ants",
		"ent",
		"ents",
		"a",
		"ez",
		"ai",
		"ais",
		"ait",
		"aient",
		"ois",
		"oit",
		"oient",
		"ent",
		"èrent"
	)
}

package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Features._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

import scala.collection.mutable

object DiscriminantTrainer extends Algorithm[Trainer] {

	def name: String = "Disc"

	override def parameters: Set[Parameter[_]] = Set(ORDER, ITERATION_COUNT, AVERAGING)

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	object AVERAGING extends BooleanParameter("Averaging", true)

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer {

		import io.github.ptitjes.hmm.Corpora._

		def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
			val breadth = stateCount(corpus)
			val depth = configuration(ORDER)
			val useAveraging = configuration(AVERAGING)

			val size = pow(breadth, depth)

			val dictionary: mutable.Map[Int, Int] = mutable.Map()

			val sequences = corpus.sequences

			sequences.foreach { s: Sequence with Annotation =>
				s.observablesAndStates.foreach { case (word, cat) =>
					if (!dictionary.contains(word))
						dictionary(word) = 1
					else
						dictionary(word) += 1
				}
			}

			val commonWords = (dictionary.view filter (_._2 > 5) map { case (o, _) => o}).toSet
			val rareWords = (dictionary.view filter (_._2 <= 5) map { case (o, _) => Lexica.WORDS(o)}).toSet

			val suffixes = (rareWords flatMap {
				w => for (s <- 1 to 4 if w.length >= s) yield w.substring(w.length - s)
			}).toSet
			val prefixes = (rareWords flatMap {
				w => for (s <- 1 to 4 if w.length >= s) yield w.substring(0, s)
			}).toSet

			val weightFactory: () => Array[Double] = () => Array.ofDim[Double](breadth)
			val features =
				FTConjunction(
					makeBigramTree(breadth, weightFactory) ::
						makeTrigramTree(breadth, weightFactory) ::
						makePrefixTree(prefixes, weightFactory) ::
						makeSuffixTree(suffixes, weightFactory) ::
						makeWordTree(commonWords, weightFactory) ::
						FTGuard(PNumber(), FTLeaf(weightFactory())) ::
						FTGuard(PCapitalized(), FTLeaf(weightFactory())) ::
						FTGuard(PContains('-'), FTLeaf(weightFactory())) ::
						Nil
				)
			//        List(FWord(WPNumber()), FWord(WPCapitalized()), FWord(WPContains('-'))) ++
			//          (for (h_1 <- allTags) yield FTag1(h_1)) ++
			//          //(for (h_1 <- allTags; h_2 <- allTags) yield FTag2(h_1, h_2)) ++
			//          (for (w <- commonWords) yield FWord(WordPredicate.makeWord(w))) ++
			//          (for (s <- suffixes) yield FWord(WordPredicate.makeSuffix(s))) ++
			//          (for (p <- prefixes) yield FWord(WordPredicate.makePrefix(p)))

			val featureInc = 1.0 // / featureCount

			val hmm = HMMDiscriminant(breadth, depth, features, dictionary)

			val decoder = FullDecoder.instantiate(configuration)
			decoder.setHmm(hmm)

			val iterationCount = configuration(ITERATION_COUNT)
			for (i <- 1 to iterationCount) {

				val count = sequences.length
				var done = 0
				sequences.foreach { refSeq: Sequence with Annotation =>

					printProgress(done, count)

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

							if (sRef != sHyp || previousRefState != previousHypState) {
								val word = Word(oRef, Lexica.WORDS(oRef))
								val h_ref = History(word, null, null,
									Array(
										if (d == 0) -1 else previousRefState % breadth,
										if (d <= 1) -1 else previousRefState / breadth % breadth
									)
								)
								val h_hyp = History(word, null, null,
									Array(
										if (d == 0) -1 else previousHypState % breadth,
										if (d <= 1) -1 else previousHypState / breadth % breadth
									)
								)

								features.foreach(h_ref)(weights => weights(sRef) += featureInc)
								features.foreach(h_hyp)(weights => weights(sHyp) -= featureInc)
							}

							if (d < depth) {
								d += 1
							}
							previousRefState = (previousRefState * breadth + sRef) % size
							previousHypState = (previousHypState * breadth + sHyp) % size
					}

					done += 1
				}

				printProgress(done, count)
			}

			hmm
		}
	}

	def printProgress(done: Int, count: Int): Unit = {
		if (done < count) {
			val doneSize = done * 100 / count
			print(f"$done%5d/$count |" + "=" * doneSize + " " * (100 - doneSize) + "|\r")
		} else print(f"$done%5d/$count |" + "=" * 100 + "|\n")
	}
}

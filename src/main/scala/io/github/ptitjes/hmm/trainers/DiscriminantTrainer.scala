package io.github.ptitjes.hmm.trainers

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Features._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

import scala.collection._

object DiscriminantTrainer extends Trainer.Factory {

	def name: String = "Disc"

	override def parameters: Set[Parameter[_]] = Set(ORDER, ITERATION_COUNT, AVERAGING)

	object DECODER extends DecoderParameter("", c => c(Configuration.DECODER))

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	val NO_AVERAGING = 0
	val PARTIAL_AVERAGING = 1
	val COMPLETE_AVERAGING = 2

	object AVERAGING extends IntParameter("Averaging", 0) {
		override def formatValue(value: Int): String = value match {
			case NO_AVERAGING => "No"
			case PARTIAL_AVERAGING => "Partial"
			case COMPLETE_AVERAGING => "Complete"
		}
	}

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer with IterativeTrainer {

		val depth = configuration(ORDER)
		val useAveraging = configuration(AVERAGING)

		def train(corpus: Corpus[Sequence with Annotation], callback: IterationCallback): Unit = {
			val breadth = stateCount(corpus)

			val dictionary: mutable.Map[Word, BitSet] = mutable.Map()
			val previousWords = Array.fill(depth) {
				mutable.Map[Word, BitSet]()
			}
			val nextWords = Array.fill(depth) {
				mutable.Map[Word, BitSet]()
			}
			val ngrams = Array.fill(depth) {
				mutable.Set[(List[Int], Int)]()
			}

			val sequences = corpus.sequences

			def addTagForWord(word: Word, tag: Int, dict: mutable.Map[Word, BitSet]) = {
				if (word != null) {
					if (!dict.contains(word))
						dict(word) = BitSet(tag)
					else
						dict(word) += tag
				}
			}

			sequences.foreach { s: Sequence with Annotation =>
				val iterator = s.annotatedIterator(breadth, depth)
				while (iterator.hasNext) {
					val (word, tag) = iterator.next()

					addTagForWord(word, tag, dictionary)

					val h = iterator.history
					(0 until depth).foreach { i =>
						addTagForWord(h.previousWords(i), tag, previousWords(i))
						addTagForWord(h.nextWords(i), tag, nextWords(i))
					}
					(0 until depth).foreach { i =>
						ngrams(i) += ((h.previousTags.toList, tag))
					}
				}
			}

			val wordsByCode = dictionary map { case (w, tags) => (w.code, tags)}
			val wordsByString = dictionary map { case (w, tags) => (w.string, tags)}

			val allTags = BitSet() ++ (0 until breadth).toSet

			var allWeightPairs = mutable.ArrayBuffer[(Weights, Weights)]()
			val weightFactory: BitSet => (Weights, Weights) = {
				tags =>
					val weights = new Weights(breadth, tags)
					val averagedWeights = new Weights(breadth, tags)
					val weightPair = (weights, averagedWeights)
					allWeightPairs += weightPair
					weightPair
			}

			val wordOnlyFeatures =
				FTConjunction(
					makePrefixTree(wordsByString, weightFactory) ::
						makeSuffixTree(wordsByString, weightFactory) ::

						FTGuard(PContainsNumber(), FTLeaf(weightFactory(allTags), allTags)) ::
						FTGuard(PContains('-'), FTLeaf(weightFactory(allTags), allTags)) ::
						FTGuard(PContainsUppercase(), FTLeaf(weightFactory(allTags), allTags)) ::
						FTGuard(PUppercaseOnly(), FTLeaf(weightFactory(allTags), allTags)) ::
						FTGuard(PContainsUppercase(),
							FTDispatchInt(EPreviousTag(1),
								(0 until breadth).map(i => (i, FTLeaf(weightFactory(allTags), allTags))).toMap)) ::

						makeWordTree(0, wordsByCode, weightFactory) :: Nil ++
						(1 to depth).map(d => makeWordTree(-d,
							previousWords(d - 1).map { case (w, tags) => (w.code, tags)},
							weightFactory)) ++
						(1 to depth).map(d => makeWordTree(d,
							nextWords(d - 1).map { case (w, tags) => (w.code, tags)},
							weightFactory))
				)

			val otherFeatures =
				FTConjunction(
					(1 to depth).map(d => makeNgramTree(ngrams(d - 1), weightFactory))
				)

			val hmm = HMMDiscriminant(breadth, depth,
				wordOnlyFeatures.map { case (weights, averagedWeights) => weights},
				otherFeatures.map { case (weights, averagedWeights) => weights},
				dictionary.map { case (word, tags) => (word.code, BitSet() ++ tags)})

			val decoder = configuration(DECODER).instantiate(hmm, configuration)

			val iterationCount = configuration(ITERATION_COUNT)
			for (i <- 1 to iterationCount) {

				val (_, elapsedTime) = timed {
					val progress = new ProgressBar(f"Iteration $i%2d/$iterationCount%2d", sequences.length)
					progress.set(0)

					sequences.foreach { refSeq: Sequence with Annotation =>

						val hypSeq = decoder.decode(refSeq)

						if (refSeq.observablesAndStates.length != hypSeq.observablesAndStates.length) {
							throw new IllegalStateException("Observable length mismatch!")
						}

						val refIterator = refSeq.annotatedIterator(breadth, depth)
						val hypIterator = hypSeq.annotatedIterator(breadth, depth)
						while (refIterator.hasNext) {
							val (oRef, sRef) = refIterator.next()
							val (oHyp, sHyp) = hypIterator.next()

							if (oRef != oHyp) {
								throw new IllegalStateException("Observable mismatch!")
							}

							if (sRef != sHyp || refIterator.sourceState != hypIterator.sourceState) {
								val h_ref = refIterator.history
								val h_hyp = hypIterator.history

								val refTagFilter = BitSet(sRef)
								val hypTagFilter = BitSet(sHyp)

								wordOnlyFeatures.foreachMatching(h_ref, refTagFilter) {
									case (weights, _) => weights(sRef) += 1.0
								}
								wordOnlyFeatures.foreachMatching(h_hyp, hypTagFilter) {
									case (weights, _) => weights(sHyp) -= 1.0
								}

								otherFeatures.foreachMatching(h_ref, refTagFilter) {
									case (weights, _) => weights(sRef) += 1.0
								}
								otherFeatures.foreachMatching(h_hyp, hypTagFilter) {
									case (weights, _) => weights(sHyp) -= 1.0
								}
							}
						}

						if (useAveraging == COMPLETE_AVERAGING) {
							allWeightPairs.par.foreach {
								case (weights, averagedWeights) =>
									weights.foreach { case (tag, weight) => averagedWeights(tag) += weight}
							}
						}

						progress.increment()
					}

					if (useAveraging == PARTIAL_AVERAGING) {
						allWeightPairs.par.foreach {
							case (weights, averagedWeights) =>
								weights.foreach { case (tag, weight) => averagedWeights(tag) += weight}
						}
					}
				}

				callback.iterationDone(configuration.set(ITERATION_COUNT, i),
					if (useAveraging == NO_AVERAGING) hmm
					else {
						val averagingDivider = i * (if (useAveraging == COMPLETE_AVERAGING) sequences.size else 1)

						HMMDiscriminant(breadth, depth,
							wordOnlyFeatures.map { case (weights, averagedWeights) =>
								averagedWeights.map(w => w / averagingDivider)
							},
							otherFeatures.map { case (weights, averagedWeights) =>
								averagedWeights.map(w => w / averagingDivider)
							},
							dictionary.map { case (word, tags) => (word.code, BitSet() ++ tags)}
						)
					},
					elapsedTime
				)
			}
		}
	}

	def printProgress(done: Int, count: Int): Unit = {
		if (done < count) {
			val doneSize = done * 100 / count
			print(f"$done%5d/$count |" + "=" * doneSize + " " * (100 - doneSize) + "|\r")
		} else print(f"$done%5d/$count |" + "=" * 100 + "|\n")
	}
}

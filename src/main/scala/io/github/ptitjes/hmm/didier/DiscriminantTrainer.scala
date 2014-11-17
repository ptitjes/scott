package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Features._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Analysis
import io.github.ptitjes.hmm.analysis.Analysis.DecoderParameter

import scala.collection.mutable

object DiscriminantTrainer extends Algorithm[Trainer] {

	def name: String = "Disc"

	override def parameters: Set[Parameter[_]] = Set(ORDER, ITERATION_COUNT, AVERAGING)

	object DECODER extends DecoderParameter("", c => c(Analysis.DECODER))

	object ITERATION_COUNT extends IntParameter("Iterations", 1)

	object AVERAGING extends BooleanParameter("Averaging", false)

	def instantiate(configuration: Configuration): Trainer = new Instance(configuration)

	class Instance(configuration: Configuration) extends Trainer {

		import io.github.ptitjes.hmm.Corpora._

		def train(corpus: Corpus[Sequence with Annotation]): HiddenMarkovModel = {
			val breadth = stateCount(corpus)
			val depth = configuration(ORDER)
			val useAveraging = configuration(AVERAGING)

			val dictionary: mutable.Map[Word, Int] = mutable.Map()

			val sequences = corpus.sequences

			sequences.foreach { s: Sequence with Annotation =>
				s.observablesAndStates.foreach { case (word, cat) =>
					if (!dictionary.contains(word))
						dictionary(word) = 1
					else
						dictionary(word) += 1
				}
			}

			val allWords = (dictionary.view map { case (w, _) => w.code}).toSet
			val commonWords = (dictionary.view /*filter (_._2 > 5)*/ map { case (w, _) => w.code}).toSet
			val rareWords = (dictionary.view /*filter (_._2 <= 5)*/ map { case (w, _) => w.string}).toSet

			val suffixes = (rareWords flatMap {
				w => for (s <- 1 to 4 if w.length >= s) yield w.substring(w.length - s)
			}).toSet
			val prefixes = (rareWords flatMap {
				w => for (s <- 1 to 4 if w.length >= s) yield w.substring(0, s)
			}).toSet

			val weightFactory: () => Array[Double] = () => Array.ofDim[Double](breadth)

			val wordOnlyFeatures =
				FTConjunction(
					makePrefixTree(prefixes, weightFactory) ::
						makeSuffixTree(suffixes, weightFactory) ::
						FTGuard(PNumber(), FTLeaf(weightFactory())) ::
						//						FTGuard(PCapitalized(), FTLeaf(weightFactory())) ::
						//						FTGuard(PContains('-'), FTLeaf(weightFactory())) ::
						makeWordTree(0, commonWords, weightFactory) :: Nil ++
						(1 to depth).map(d => makeWordTree(-d, allWords, weightFactory)) ++
						(1 to depth).map(d => makeWordTree(d, allWords, weightFactory))
				)

			val otherFeatures =
				FTConjunction(
					(1 to depth).map(d => makeNgramTree(d, breadth, weightFactory))
				)

			val wordOnlyFeaturesAvg =
				FTConjunction(
					makePrefixTree(prefixes, weightFactory) ::
						makeSuffixTree(suffixes, weightFactory) ::
						FTGuard(PNumber(), FTLeaf(weightFactory())) ::
						//						FTGuard(PCapitalized(), FTLeaf(weightFactory())) ::
						//						FTGuard(PContains('-'), FTLeaf(weightFactory())) ::
						makeWordTree(0, commonWords, weightFactory) :: Nil ++
						(1 to depth).map(d => makeWordTree(-d, allWords, weightFactory)) ++
						(1 to depth).map(d => makeWordTree(d, allWords, weightFactory))
				)

			val otherFeaturesAvg =
				FTConjunction(
					(1 to depth).map(d => makeNgramTree(d, breadth, weightFactory))
				)

			val hmm = HMMDiscriminant(breadth, depth,
				wordOnlyFeatures, otherFeatures, dictionary.map { case (w, c) => (w.code, c)})

			val decoder = configuration(DECODER).instantiate(configuration)
			decoder.setHmm(hmm)

			val sequencesCount = sequences.size
			val iterationCount = configuration(ITERATION_COUNT)
			for (i <- 1 to iterationCount) {

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

							wordOnlyFeatures.foreach(h_ref)(weights => weights(sRef) += 1.0)
							wordOnlyFeatures.foreach(h_hyp)(weights => weights(sHyp) -= 1.0)

							otherFeatures.foreach(h_ref)(weights => weights(sRef) += 1.0)
							otherFeatures.foreach(h_hyp)(weights => weights(sHyp) -= 1.0)
						}
					}

					progress.increment()
				}

				if (useAveraging) {
					wordOnlyFeaturesAvg.addAveraged(wordOnlyFeatures, iterationCount)
					otherFeaturesAvg.addAveraged(otherFeatures, iterationCount)
				}
			}

			if (!useAveraging) hmm
			else HMMDiscriminant(breadth, depth,
				wordOnlyFeaturesAvg, otherFeaturesAvg, dictionary.map { case (w, c) => (w.code, c)})
		}
	}

	def printProgress(done: Int, count: Int): Unit = {
		if (done < count) {
			val doneSize = done * 100 / count
			print(f"$done%5d/$count |" + "=" * doneSize + " " * (100 - doneSize) + "|\r")
		} else print(f"$done%5d/$count |" + "=" * 100 + "|\n")
	}
}

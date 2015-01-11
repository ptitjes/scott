package io.github.ptitjes.scott.nl.lang.fr

import java.io.File

import io.github.ptitjes.scott.api.HiddenMarkovModel._
import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.decoders.BeamDecoder
import io.github.ptitjes.scott.nl.analysis.Checking
import io.github.ptitjes.scott.nl.corpora.Corpora._
import io.github.ptitjes.scott.nl.features.BaseFeatures
import io.github.ptitjes.scott.trainers.DiscriminantTrainer
import io.github.ptitjes.scott.utils.Utils._

object trainFTBCoarse extends App {

	val ftbPath = args(0)

	val (trainCorpus, devCorpus, testCorpus) = FTB.parseSplitCoarse(ftbPath)

	val trainer = new DiscriminantTrainer[NLToken, NLToken with NLPosTag](
		order = 2,
		iterationCount = 15,
		useAveraging = DiscriminantTrainer.COMPLETE_AVERAGING,
		features = BaseFeatures,
		FTB.wordCodeExtractor,
		FTB.tagExtractor,
		FTB.coarseTokenBuilder
	)

	trainer.train(trainCorpus, new IterationCallback[NLToken, NLToken with NLPosTag] {
		override def iterationDone(iteration: Int, hmm: HiddenMarkovModel[NLToken, NLToken with NLPosTag], elapsedTime: Long): Unit = {
			val hmmName = "FTB-Coarse-" + iteration
			val hmmFile = new File("temp/" + hmmName + ".hmm")

			decode(hmm, hmmName)

			timed("Saving model") {
				writeTo(hmm, hmmFile)
			}
			val (loadedHmm, _) = timed("Loading model") {
				readFrom[NLToken, NLToken with NLPosTag](hmmFile)
			}

			decode(loadedHmm, "Loaded-" + hmmName)

			println()
		}
	})

	def decode(hmm: HiddenMarkovModel[NLToken, NLToken with NLPosTag], hmmName: String) {
		val decoder = new BeamDecoder(hmm)
		val hypCorpus = decoder.decode(devCorpus)
		Checking.check(hmm, devCorpus, hypCorpus, new File("temp/Decode-on-" + hmmName + ".check")).display()
	}
}

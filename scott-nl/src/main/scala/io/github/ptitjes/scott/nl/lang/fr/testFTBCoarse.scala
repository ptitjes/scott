package io.github.ptitjes.scott.nl.lang.fr

import java.io.File

import io.github.ptitjes.scott.api.HiddenMarkovModel._
import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.decoders.BeamDecoder
import io.github.ptitjes.scott.nl.analysis.Checking
import io.github.ptitjes.scott.nl.conll.{CoNLLCoarseToken, CoNLLXParser}
import io.github.ptitjes.scott.nl.corpora.Corpora._
import io.github.ptitjes.scott.nl.corpora.Lexica
import io.github.ptitjes.scott.nl.features.BaseFeatures
import io.github.ptitjes.scott.trainers.DiscriminantTrainer
import io.github.ptitjes.scott.utils.Utils._

import scala.io.Source

object testFTBCoarse extends App {

	val PATH_TO_FTB = "/home/didier/Documents/Work/Master/DM/InfStat/ftb"
	val CONLL_CORPUS = "ftb4+mc+undocpd+fct+structmod110908"

	val trainCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_1.dep_conll"
	val devCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_2.dep_conll"
	val testCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_3.dep_conll"

	val parser = new CoNLLXParser
	val profile = FTB.CoNLLProfile
	val trainCorpus = parser.parseCoarse(profile, Source.fromFile(trainCorpusPath), Lexica.WORDS)
	val devCorpus = parser.parseCoarse(profile, Source.fromFile(devCorpusPath), Lexica.WORDS)
	val testCorpus = parser.parseCoarse(profile, Source.fromFile(testCorpusPath), Lexica.WORDS)

	val trainer = new DiscriminantTrainer[NLToken, NLToken with NLPosTag](
		order = 2,
		iterationCount = 10,
		useAveraging = DiscriminantTrainer.COMPLETE_AVERAGING,
		features = BaseFeatures,
		_.word.code,
		_.tag,
		(token, tag) => CoNLLCoarseToken(token.word, tag)
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
		Checking.check(hmm, devCorpus, hypCorpus, devCorpus.tagSet, new File("temp/Decode-on-" + hmmName + ".check")).display()
	}
}

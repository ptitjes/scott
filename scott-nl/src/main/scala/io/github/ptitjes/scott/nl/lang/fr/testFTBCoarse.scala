package io.github.ptitjes.scott.nl.lang.fr

import java.io.File

import io.github.ptitjes.scott.HiddenMarkovModel._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.analysis.Checking
import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.decoders.BeamDecoder
import io.github.ptitjes.scott.nl.conll.{CoNLLToken, CoNLLXParser}
import io.github.ptitjes.scott.trainers.DiscriminantTrainer
import io.github.ptitjes.scott.trainers.features.BaseFeatures

import scala.io.Source

object testFTBCoarse extends App {

	val PATH_TO_FTB = "/home/didier/Documents/Work/Master/DM/InfStat/ftb"
	val CONLL_CORPUS = "ftb4+mc+undocpd+fct+structmod110908"

	val trainCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_1.dep_conll"
	val devCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_2.dep_conll"
	val testCorpusPath = PATH_TO_FTB + "/corpus-conll/" + CONLL_CORPUS + "_3.dep_conll"

	val parser = new CoNLLXParser
	val profile = FTB.CoNLLProfile
	val tagSet = profile.coarseTagSet
	val trainCorpus = downgrade(parser.parse(profile, Source.fromFile(trainCorpusPath), Lexica.WORDS))
	val devCorpus = downgrade(parser.parse(profile, Source.fromFile(devCorpusPath), Lexica.WORDS))
	val testCorpus = downgrade(parser.parse(profile, Source.fromFile(testCorpusPath), Lexica.WORDS))

	val trainer = new DiscriminantTrainer[NLToken, NLToken with NLPosTag](
		order = 2,
		iterationCount = 20,
		useAveraging = DiscriminantTrainer.COMPLETE_AVERAGING,
		features = BaseFeatures,
		_.word.code,
		_.tag,
		(token, tag) => AnnotatedNLToken(token.word, tag)
	)

	trainer.train(trainCorpus, new IterationCallback[NLToken, NLToken with NLPosTag] {
		override def iterationDone(iteration: Int, hmm: HiddenMarkovModel[NLToken, NLToken with NLPosTag], elapsedTime: Long): Unit = {
			val decoder = new BeamDecoder(hmm)
			val hypCorpus = decoder.decode(devCorpus)

			val hmmName = "FTB-Coarse-" + iteration

			Checking.check(hmm, devCorpus, hypCorpus, tagSet, new File("temp/Decode-on-" + hmmName + ".check")).display()

			timed("Saving model") {
				writeTo(hmm, new File("temp/" + hmmName + ".hmm"))
			}
		}
	})

	def downgrade(corpus: Corpus[CoNLLToken]) =
		BasicCorpus(
			corpus.sequences.map(s => BaseSequence(s.tokens.map(t => AnnotatedNLToken(t.word, t.coarseTag)))),
			FTB.CoarsePosTags
		)
}

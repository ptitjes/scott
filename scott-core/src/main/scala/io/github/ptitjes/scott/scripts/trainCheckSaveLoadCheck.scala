package io.github.ptitjes.scott.scripts

import java.io.File

import io.github.ptitjes.scott.HiddenMarkovModel._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.analysis.Checking
import io.github.ptitjes.scott.corpora._
import io.github.ptitjes.scott.decoders.{BeamDecoder, FullDecoder}
import io.github.ptitjes.scott.trainers.DiscriminantTrainer
import io.github.ptitjes.scott.trainers.features.BaseFeatures

object trainCheckSaveLoadCheck extends App {

	val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS, Lexica.CATEGORIES)
	val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS, Lexica.CATEGORIES)

	val trainer = new DiscriminantTrainer[NLToken, NLToken with NLPosTag](
		order = 2,
		iterationCount = 10,
		useAveraging = DiscriminantTrainer.COMPLETE_AVERAGING,
		features = BaseFeatures,
		_.word.code,
		_.tag,
		(token, tag) => AnnotatedNLToken(token.word, tag)
	)

	trainer.train(trainCorpus, new IterationCallback[NLToken, NLToken with NLPosTag] {
		override def iterationDone(iteration: Int, hmm: HiddenMarkovModel[NLToken, NLToken with NLPosTag], elapsedTime: Long): Unit = {
			val decoder = new BeamDecoder[NLToken, NLToken with NLPosTag](hmm)
			val hypCorpus = decoder.decode(devCorpus)

			val results = Checking.check(hmm, devCorpus, hypCorpus, Lexica.CATEGORIES,
				new File("temp/Decode-on-Iteration-" + iteration + ".check"))

			results.display()
		}
	})
}

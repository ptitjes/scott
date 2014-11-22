package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Trainer._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.analysis.ResultPool._

import scala.collection.mutable

class AnalysisRunner(cacheFilename: String,
                     trainCorpus: Corpus[Sequence with Annotation],
                     testCorpus: Corpus[Sequence with Annotation],
                     force: Boolean = false) {

	private val cacheFile = new File(cacheFilename)

	private var pool = if (!cacheFile.exists()) ResultPool() else loadResults(cacheFile)
	private var hmms = mutable.Map[Configuration, HiddenMarkovModel]()

	import io.github.ptitjes.hmm.Configuration._

	def resultsFor(configurations: ConfigurationSet): ResultPool = {
		for (
			c <- configurations.generate();
			conf = complete(c);
			trainerFactory = c(TRAINER);
			decoderFactory = c(DECODER)
		) {
			if (force || !pool.results.contains(conf)) {
				println(s"Running " + conf)

				val corpusRatio = c(CORPUS_RATIO).toDouble / 100
				val trainCorpusPart = trainCorpus.splitBy(corpusRatio)._1

				val trainer = trainerFactory.instantiate(conf)

				if (trainer.isInstanceOf[IterativeTrainer]) {
					val iterativeTrainer = trainer.asInstanceOf[IterativeTrainer]

					var totalElapsedTime: Long = 0
					iterativeTrainer.train(trainCorpus, new IterationCallback {
						def iterationDone(configuration: Configuration, hmm: HiddenMarkovModel, elapsedTime: Long): Unit = {
							totalElapsedTime += elapsedTime
							testAndSaveHmm(configuration, hmm, totalElapsedTime)
						}
					})
				} else {
					val (hmm, trainingElapsedTime) = timed {
						trainer.train(trainCorpusPart)
					}

					testAndSaveHmm(conf, hmm, trainingElapsedTime)
				}

				def testAndSaveHmm(configuration: Configuration, hmm: HiddenMarkovModel, trainingElapsedTime: Long) {
					//toFile(hmm, new File("results/hmms/" + configuration.toFilename + ".json"))

					val decoder = decoderFactory.instantiate(hmm, configuration)
					val (hypCorpus, decodingElapsedTime) = timed {
						decoder.decode(testCorpus)
					}

					val results = Checking.check(configuration, hmm, testCorpus, hypCorpus,
						trainingElapsedTime, decodingElapsedTime,
						new File("results/checks/" + configuration.toFilename + ".check"))

					println("\t" + results)

					pool = pool(configuration) = results
					saveResults(cacheFile, pool)
				}
			}
			println()
		}

		pool
	}
}

object Analysis {

}

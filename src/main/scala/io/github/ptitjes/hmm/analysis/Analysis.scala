package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.analysis.ResultPool._
import io.github.ptitjes.hmm.analysis.Results._

class AnalysisRunner(cacheFilename: String,
                     trainCorpus: Corpus[Sequence with Annotation],
                     testCorpus: Corpus[Sequence with Annotation],
                     force: Boolean = false) {

	private val cacheFile = new File(cacheFilename)

	private var pool = if (!cacheFile.exists()) ResultPool() else loadResults(cacheFile)

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
				val (hmm, trainingElapsedTime) = timed {
					trainer.train(trainCorpusPart)
				}

				val decoder = decoderFactory.instantiate(hmm, conf)
				val (hypCorpus, decodingElapsedTime) = timed {
					decoder.decode(testCorpus)
				}

				val results = check(hmm, testCorpus, hypCorpus,
					trainingElapsedTime, decodingElapsedTime, debug = false)

				println("\t" + results)

				pool = pool(conf) = results
				saveResults(cacheFile, pool)
			}
		}

		pool
	}
}

object Analysis {

}

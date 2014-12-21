package io.github.ptitjes.scott.analysis

import java.io._

import io.github.ptitjes.scott.HiddenMarkovModel._
import io.github.ptitjes.scott.Trainer._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.Corpora._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott.analysis.AnalysisPool._

import scala.collection.mutable

class AnalysisRunner(cacheFilename: String,
                     trainCorpus: Corpus[Sequence with Annotation],
                     testCorpus: Corpus[Sequence with Annotation],
                     force: Boolean = false) {

	private val cacheFile = new File(cacheFilename)
	private val pool = new AnalysisPool(cacheFile)

	import io.github.ptitjes.scott.Configuration._

	def resultsFor[X](common: ConfigurationSet,
	                  columns: ConfigurationSet,
	                  rows: MultiValuedParameter[X],
	                  f: (X, Results) => Double): (List[Configuration], List[(X, List[Double])]) = {

		computeResults(columns * rows * common)

		val columnConfigurations = columns.generate()
		val rowConfigurations = rows.generate()
		val rowParameter = rows.parameter

		val data = for (row <- rowConfigurations) yield {
			val rowValue = row(rowParameter)
			(rowValue, columnConfigurations.map { column =>
				val conf = common.flatten + column + row
				val trainingConf = conf.completeForTraining
				val decodingConf = conf.completeForDecoding
				val (results, _) = pool.getDecoding((trainingConf, decodingConf))
				f(rowValue, results)
			})
		}
		(columnConfigurations, data)
	}

	def resultsFor[X](common: ConfigurationSet,
	                  columns: ConfigurationSet,
	                  rows: List[X],
	                  f: (X, Results) => Double): (List[Configuration], List[(X, List[Double])]) = {

		computeResults(columns * common)

		val columnConfigurations = columns.generate()

		val data = for (row <- rows) yield {
			(row, columnConfigurations.map { column =>
				val conf = common.flatten() + column
				val trainingConf = conf.completeForTraining
				val decodingConf = conf.completeForDecoding
				val (results, _) = pool.getDecoding((trainingConf, decodingConf))
				f(row, results)
			})
		}
		(columnConfigurations, data)
	}

	def computeResults(configurations: ConfigurationSet): AnalysisPool = {
		for {
			conf <- configurations.generate()
			trainingConf = conf.completeForTraining
			decodingConf = conf.completeForDecoding
			trainerFactory = conf(TRAINER)
			decoderFactory = conf(DECODER)
		} {
			if (force || !pool.hasDecoding((trainingConf, decodingConf))) {

				if (trainerFactory.isIterative) {
					if (force || !pool.hasTraining(trainingConf)) {
						println(s"Running training : " + trainingConf)

						val trainer = trainerFactory.instantiate(trainingConf)
						val trainCorpusPart = buildCorpus(trainingConf)

						val iterativeTrainer = trainer.asInstanceOf[IterativeTrainer]

						var totalElapsedTime: Long = 0
						iterativeTrainer.train(trainCorpusPart, new IterationCallback {
							def iterationDone(configuration: Configuration, hmm: HiddenMarkovModel, elapsedTime: Long): Unit = {
								totalElapsedTime += elapsedTime
								pool.addTraining(configuration, hmm, elapsedTime)

								test(configuration, hmm)
							}
						})
					} else {
						val (hmm, _) = pool.getTraining(trainingConf)

						test(trainingConf, hmm)
					}
				} else {
					val (hmm, _) =
						if (force || !pool.hasTraining(trainingConf)) {
							println(s"Running training : " + trainingConf)

							val trainer = trainerFactory.instantiate(trainingConf)
							val trainCorpusPart = buildCorpus(trainingConf)
							val (hmm, elapsedTime) = timed {
								trainer.train(trainCorpusPart)
							}

							pool.addTraining(trainingConf, hmm, elapsedTime)
							(hmm, elapsedTime)
						} else pool.getTraining(trainingConf)

					test(trainingConf, hmm)
				}

				def test(trainingConf: Configuration, hmm: HiddenMarkovModel) {
					if (force || !pool.hasDecoding((trainingConf, decodingConf))) {
						println(s"Running decoding " + decodingConf + " on " + trainingConf)

						val decoder = decoderFactory.instantiate(hmm, decodingConf)
						val (hypCorpus, elapsedTime) = timed {
							decoder.decode(testCorpus)
						}

						val results = Checking.check(decodingConf, hmm, testCorpus, hypCorpus,
							new File("analysis/checks/" + trainingConf.toFilename + "-with-" + decodingConf.toFilename + ".check"))

						println("\t" + results)

						pool.addDecoding((trainingConf, decodingConf), results, elapsedTime)
					}
				}
				println()
			}
		}

		pool
	}

	def buildCorpus(c: Configuration) = {
		val corpusRatio = c(CORPUS_RATIO).toDouble / 100
		trainCorpus.splitBy(corpusRatio)._1
	}
}

object Analysis {

}

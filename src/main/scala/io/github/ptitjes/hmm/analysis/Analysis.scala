package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ResultPool._
import io.github.ptitjes.hmm.analysis.Results._
import io.github.ptitjes.hmm.decoders.FullDecoder
import io.github.ptitjes.hmm.trainers.RelFreqTrainer
import org.json4s._

import scala.collection._

class AnalysisRunner(cacheFilename: String,
                     trainCorpus: Corpus[Sequence with Annotation],
                     testCorpus: Corpus[Sequence with Annotation],
                     force: Boolean = false) {

	private val cacheFile = new File(cacheFilename)

	private val trainerPool = mutable.Map[Configuration, Trainer]()
	private val decoderPool = mutable.Map[Configuration, Decoder]()

	private var pool = if (!cacheFile.exists()) ResultPool() else loadResults(cacheFile)

	import io.github.ptitjes.hmm.analysis.Analysis._

	def resultsFor(configurations: ConfigurationSet): ResultPool = {
		for (
			c <- configurations.generate();
			conf = completeConfiguration(c);
			ta = c(TRAINER);
			da = c(DECODER)
		) {
			if (force || !pool.results.contains(conf)) {
				val trainer = {
					if (!trainerPool.contains(conf)) {
						trainerPool(conf) = ta.instantiate(conf)
					}
					trainerPool(conf)
				}

				val decoder = {
					if (!decoderPool.contains(conf)) {
						decoderPool(conf) = da.instantiate(conf)
					}
					decoderPool(conf)
				}

				println(s"Running " + conf)

				val corpusRatio = c(CORPUS_RATIO).toDouble / 100

				val r = trainDecodeAndCheck(trainer, decoder,
					trainCorpus.splitBy(corpusRatio)._1, testCorpus)

				println("\t" + r)

				pool = pool(conf) = r
				saveResults(cacheFile, pool)
			}
		}

		pool
	}
}

object Analysis {

	object CORPUS_RATIO extends IntParameter("Corpus Ratio", 100)

	object TRAINER extends TrainerParameter("", c => RelFreqTrainer)

	object DECODER extends DecoderParameter("", c => FullDecoder)

	def completeConfiguration(configuration: Configuration): Configuration = {

		val ta = configuration(TRAINER)
		val da = configuration(DECODER)

		(List(CORPUS_RATIO) ++ ta.parameters ++ da.parameters).foldLeft(configuration) {
			case (c, param) => if (c.parameters.contains(param)) c else c.set(param, param.default(c))
		}
	}

	case class TrainerParameter(name: String, default: Configuration => Algorithm[Trainer])
		extends Parameter[Algorithm[Trainer]]() {

		def formatValue(value: Algorithm[Trainer]): String = value.name

		def fromJson(value: JValue): Algorithm[Trainer] = value match {
			case JString(t) => nameToObject[Algorithm[Trainer]](t)
			case _ => throw new IllegalArgumentException
		}

		def toJson(value: Algorithm[Trainer]): JValue = JString(objectToName(value))
	}

	case class DecoderParameter(name: String, default: Configuration => Algorithm[Decoder])
		extends Parameter[Algorithm[Decoder]]() {

		def formatValue(value: Algorithm[Decoder]): String = value.name

		def fromJson(value: JValue): Algorithm[Decoder] = value match {
			case JString(t) => nameToObject[Algorithm[Decoder]](t)
			case _ => throw new IllegalArgumentException
		}

		def toJson(value: Algorithm[Decoder]): JValue = JString(objectToName(value))
	}

}

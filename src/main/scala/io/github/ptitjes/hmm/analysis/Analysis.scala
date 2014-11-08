package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.ResultPool._
import io.github.ptitjes.hmm.analysis.Results._
import org.json4s._

import scala.collection._

class AnalysisRunner(cacheFilename: String,
                     trainCorpus: Corpus[Sequence with Annotation],
                     testCorpus: Corpus[Sequence with Annotation]) {

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
      if (!pool.results.contains(conf)) {
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

        val hmm = trainer.train(trainCorpus)
        val r = decodeAndCheck(decoder, hmm, testCorpus)

        println("\t" + r)

        pool = pool(conf) = r
        saveResults(cacheFile, pool)
      }
    }

    pool
  }
}

object Analysis {

  object TRAINER extends Parameter[Algorithm[Trainer]]() {

    def name: String = ""

    def default: Algorithm[Trainer] = didier.RelFreqTrainer

    def formatValue(value: Algorithm[Trainer]): String = value.name

    def fromJson(value: JValue): Algorithm[Trainer] = value match {
      case JString(t) => nameToObject[Algorithm[Trainer]](t)
      case _ => throw new IllegalArgumentException
    }

    def toJson(value: Algorithm[Trainer]): JValue = JString(objectToName(value))
  }

  object DECODER extends Parameter[Algorithm[Decoder]]() {

    def name: String = ""

    def default: Algorithm[Decoder] = didier.FullMTDecoder

    def formatValue(value: Algorithm[Decoder]): String = value.name

    def fromJson(value: JValue): Algorithm[Decoder] = value match {
      case JString(t) => nameToObject[Algorithm[Decoder]](t)
      case _ => throw new IllegalArgumentException
    }

    def toJson(value: Algorithm[Decoder]): JValue = JString(objectToName(value))
  }

  def completeConfiguration(configuration: Configuration): Configuration = {

    val ta = configuration(TRAINER)
    val da = configuration(DECODER)

    (ta.parameters ++ da.parameters).foldLeft(configuration) {
      case (c, param) => if (c.parameters.contains(param)) c else c.set(param, param.default)
    }
  }
}

package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm._
import org.json4s._

import scala.annotation.tailrec
import scala.collection._

case class Analysis(parameters: List[Parameter[_]] = List(), allValues: Map[Parameter[_], List[_]] = Map()) {

  def forAll[V](parameter: Parameter[V], values: V*): Analysis =
    forAll(parameter, values.toList)

  def forAll(parameter: Parameter[Int], values: Range): Analysis =
    forAll(parameter, values.toList)

  def forAll[V](parameter: Parameter[V], values: List[V]): Analysis =
    Analysis(parameters :+ parameter, allValues + (parameter -> values))

  def apply[V](parameter: Parameter[V]): List[V] = allValues(parameter).asInstanceOf[List[V]]
}

object Analysis {

  import io.github.ptitjes.hmm.analysis.ResultPool._
  import io.github.ptitjes.hmm.analysis.Results._

  object ALGORITHMS extends Parameter[(Algorithm[Trainer], Algorithm[Decoder])]() {

    def name: String = ""

    def default: (Algorithm[Trainer], Algorithm[Decoder]) = (didier.RelFreqTrainer, didier.FullMTDecoder)

    def formatValue(value: (Algorithm[Trainer], Algorithm[Decoder])): String =
      value._1.name + " + " + value._2.name

    def fromJson(value: JValue): (Algorithm[Trainer], Algorithm[Decoder]) = value match {
      case JObject(JField("_1", JString(t)) :: JField("_2", JString(d)) :: Nil) =>
        (nameToObject[Algorithm[Trainer]](t), nameToObject[Algorithm[Decoder]](d))
      case _ => throw new IllegalArgumentException
    }

    def toJson(value: (Algorithm[Trainer], Algorithm[Decoder])): JValue =
      JObject(JField("_1", JString(objectToName(value._1))) :: JField("_2", JString(objectToName(value._2))) :: Nil)
  }

  def run(trainCorpus: Corpus[Sequence with Annotation],
          testCorpus: Corpus[Sequence with Annotation],
          analysis: List[Analysis]): ResultPool = {

    val resultFile = new File("temp/results.json")

    val trainerPool = mutable.Map[Configuration, Trainer]()
    val decoderPool = mutable.Map[Configuration, Decoder]()

    var pool = if (!resultFile.exists()) ResultPool() else loadResults(resultFile)

    for (
      a <- analysis;
      c <- generateConfigurations(a);
      conf = completeConfiguration(c);
      ta = c(ALGORITHMS)._1;
      da = c(ALGORITHMS)._2
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
        saveResults(resultFile, pool)
      }
    }

    pool
  }

  @tailrec def generateConfigurations(analysis: Analysis,
                                      selected: List[Parameter[_]],
                                      base: List[Configuration]): List[Configuration] = {

    def applyParameter[V](base: List[Configuration], param: Parameter[V]): List[Configuration] = {
      base.flatMap(c => analysis(param).map(v => c.set(param, v)))
    }

    selected match {
      case param :: tail =>
        generateConfigurations(analysis, tail, applyParameter(base, param))
      case Nil => base
    }
  }

  def generateConfigurations(analysis: Analysis, selected: List[Parameter[_]]): List[Configuration] =
    generateConfigurations(analysis, selected, List(Configuration()))

  def generateConfigurations(analysis: Analysis): List[Configuration] =
    generateConfigurations(analysis, analysis.parameters)

  def completeConfiguration(configuration: Configuration): Configuration = {

    val ta = configuration(ALGORITHMS)._1
    val da = configuration(ALGORITHMS)._2

    (ta.parameters ++ da.parameters).foldLeft(configuration) {
      case (c, param) => if (c.parameters.contains(param)) c else c.set(param, param.default)
    }
  }
}

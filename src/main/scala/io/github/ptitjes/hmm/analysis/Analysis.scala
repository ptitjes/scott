package io.github.ptitjes.hmm.analysis

import java.io.PrintWriter

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.Corpora._

import scala.annotation.tailrec
import scala.collection._

trait Analysis {

  def configurations: AnalysisConfigurations
}

object Analysis {

  val ALGORITHMS = new Parameter[(Algorithm[Trainer], Algorithm[Decoder])]() {

    def name: String = ""

    def default: (Algorithm[Trainer], Algorithm[Decoder]) = (didier.RelFreqSimpleTrainer, didier.ParDecoder)

    def formatValue(value: (Algorithm[Trainer], Algorithm[Decoder])): String =
      value._1.name + " + " + value._2.name
  }

  def run(analysis: List[Analysis],
          trainCorpus: Corpus[Sequence with Annotation],
          testCorpus: Corpus[Sequence with Annotation]): ResultPool = {

    val trainerPool = mutable.Map[Configuration, Trainer]()
    val decoderPool = mutable.Map[Configuration, Decoder]()
    val results = mutable.Map[Configuration, Results]()

    for (
      a <- analysis;
      c <- a.configurations.generate
    ) {
      if (!results.contains(c)) {
        val trainer = {
          if (!trainerPool.contains(c)) {
            trainerPool(c) = c(ALGORITHMS)._1.instantiate(c)
          }
          trainerPool(c)
        }

        val decoder = {
          if (!decoderPool.contains(c)) {
            decoderPool(c) = c(ALGORITHMS)._2.instantiate(c)
          }
          decoderPool(c)
        }

        println(s"Running " + c)

        val hmm = trainer.train(15, trainCorpus)
        val r = decoder.decodeAndCheck(hmm, testCorpus)
        results(c) = r

        println("\t" + r)
      }
    }
    new ResultPool(results)
  }
}

case class AnalysisConfigurations(parameters: Map[Parameter[_], List[_]] = Map()) {

  def set[V](parameter: Parameter[V], values: List[V]): AnalysisConfigurations =
    AnalysisConfigurations(parameters + (parameter -> values))

  def apply[V](parameter: Parameter[V]): List[V] = parameters(parameter).asInstanceOf[List[V]]

  def generate: List[Configuration] = {
    def applyParameters[V](base: List[Configuration], param: Parameter[V]): List[Configuration] = {
      base.flatMap(c => this(param).map(v => c.set(param, v)))
    }

    @tailrec def generateConfigurations(paramList: List[Parameter[_]], base: List[Configuration]): List[Configuration] =
      paramList match {
        case param :: tail =>
          generateConfigurations(tail, applyParameters(base, param))
        case Nil => base
      }

    generateConfigurations(parameters.keySet.toList, List(Configuration()))
  }
}

class ResultPool(results: mutable.Map[Configuration, Results]) {

  def extractData[X](analysis: Analysis,
                     rows: Parameter[X],
                     columns: List[Configuration]): List[(X, List[Double])] = {

    for (row <- analysis.configurations(rows))
    yield (row, columns.map { c => results(c.set(rows, row)).accuracy})
  }

}

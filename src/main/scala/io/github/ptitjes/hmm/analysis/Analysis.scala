package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.Corpora._

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

  val ALGORITHMS = new Parameter[(Algorithm[Trainer], Algorithm[Decoder])]() {

    def name: String = ""

    def default: (Algorithm[Trainer], Algorithm[Decoder]) = (didier.RelFreqSimpleTrainer, didier.ParDecoder)

    def formatValue(value: (Algorithm[Trainer], Algorithm[Decoder])): String =
      value._1.name + " + " + value._2.name
  }

  def run(trainCorpus: Corpus[Sequence with Annotation],
          testCorpus: Corpus[Sequence with Annotation],
          analysis: List[Analysis]): ResultPool = {

    val trainerPool = mutable.Map[Configuration, Trainer]()
    val decoderPool = mutable.Map[Configuration, Decoder]()
    val results = mutable.Map[Configuration, Results]()

    for (
      a <- analysis;
      c <- generateConfigurations(a)
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

  @tailrec def generateConfigurations(analysis: Analysis,
                                      selected: List[Parameter[_]],
                                      base: List[Configuration]): List[Configuration] = {

    def applyParameters[V](base: List[Configuration], param: Parameter[V]): List[Configuration] = {
      base.flatMap(c => analysis(param).map(v => c.set(param, v)))
    }

    selected match {
      case param :: tail =>
        generateConfigurations(analysis, tail, applyParameters(base, param))
      case Nil => base
    }
  }

  def generateConfigurations(analysis: Analysis, selected: List[Parameter[_]]): List[Configuration] =
    generateConfigurations(analysis, selected, List(Configuration()))

  def generateConfigurations(analysis: Analysis): List[Configuration] =
    generateConfigurations(analysis, analysis.parameters)
}

class ResultPool(results: mutable.Map[Configuration, Results]) {

  def buildColumns[X](analysis: Analysis, rows: Parameter[X]): List[Configuration] =
    Analysis.generateConfigurations(analysis,
      analysis.parameters.filter {
        case `rows` => false
        case _ => true
      })

  def extractData[X](analysis: Analysis,
                     rows: Parameter[X],
                     columns: List[Configuration]): List[(X, List[Double])] = {

    for (row <- analysis(rows))
    yield (row, columns.map { c => results(c.set(rows, row)).accuracy})
  }

}

package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.Corpora._

import scala.annotation.tailrec
import scala.collection._

trait Analysis {

  def configurations: AnalysisConfigurations

  def trainers: List[Algorithm[Trainer]]

  def decoders: List[Algorithm[Decoder]]
}

object Analysis {

  def run(analysis: List[Analysis],
          trainCorpus: Corpus[Sequence with Annotation],
          testCorpus: Corpus[Sequence with Annotation]) = {

    val trainerPool = mutable.Map[(Algorithm[Trainer], Configuration), Trainer]()
    val decoderPool = mutable.Map[(Algorithm[Decoder], Configuration), Decoder]()
    val resultPool = mutable.Map[(Algorithm[Trainer], Algorithm[Decoder], Configuration), Results]()

    for (
      a <- analysis;
      c <- a.configurations.generate;
      ta <- a.trainers;
      da <- a.decoders
    ) {
      if (!resultPool.contains((ta, da, c))) {
        val trainer = {
          if (!trainerPool.contains((ta, c))) {
            trainerPool((ta, c)) = ta.instantiate(c)
          }
          trainerPool((ta, c))
        }

        val decoder = {
          if (!decoderPool.contains((da, c))) {
            decoderPool((da, c)) = da.instantiate(c)
          }
          decoderPool((da, c))
        }

        val hmm = trainer.train(15, trainCorpus)
        val results = decoder.decodeAndCheck(hmm, testCorpus)
        resultPool((ta, da, c)) = results
      }

      println(s"Algorithm= ${ta.name} + ${da.name}")
      println("\t" + a.configurations.parameters.keys.map(p => s"${p.name} = ${c(p)}").mkString("; "))
      println("\t" + resultPool((ta, da, c)))
    }
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

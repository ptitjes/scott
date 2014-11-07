package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.Corpora._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._

import scala.annotation.tailrec
import scala.collection._

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, writePretty}

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

  object ALGORITHMS extends Parameter[(Algorithm[Trainer], Algorithm[Decoder])]() {

    def name: String = ""

    def default: (Algorithm[Trainer], Algorithm[Decoder]) = (didier.RelFreqSimpleTrainer, didier.ParDecoder)

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
      c <- generateConfigurations(a)
    ) {
      if (!pool.results.contains(c)) {
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

        println("\t" + r)

        pool = pool(c) = r
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

  implicit val formats = Serialization.formats(NoTypeHints) +
    new ResultPoolSerializer +
    new ConfigurationSerializer

  def loadResults(file: File): ResultPool = {
    using(new FileReader(file)) {
      fileReader => using(new BufferedReader(fileReader)) {
        reader => read[ResultPool](reader)
      }
    }
  }

  def saveResults(file: File, results: ResultPool): Unit = {
    using(new FileWriter(file)) {
      fileOutput => using(new PrintWriter(fileOutput)) {
        out => writePretty(results, out)
      }
    }
  }

  class ResultPoolSerializer extends CustomSerializer[ResultPool](format => ( {
    case JArray(resultArray) =>
      resultArray.foldLeft(ResultPool()) {
        case (pool, JObject(JField("configuration", conf) :: JField("results.json", results) :: Nil)) =>
          pool(Extraction.extract[Configuration](conf)) = Extraction.extract[Results](results)
      }
  }, {
    case pool: ResultPool =>
      JArray(pool.results.toList.map {
        case (conf, results) =>
          JObject(JField("configuration", Extraction.decompose(conf)) ::
            JField("results.json", Extraction.decompose(results)) :: Nil)
      })
  }
    ))

  class ConfigurationSerializer extends CustomSerializer[Configuration](format => ( {
    case JObject(fields) =>
      fields.foldLeft(Configuration()) {
        case (conf, JField(key, value)) =>
          val param = nameToObject[Parameter[_]](key)

          def applyParameter[V](param: Parameter[V]): Configuration = {
            conf.set(param, param.fromJson(value))
          }

          applyParameter(param)
      }
  }, {
    case conf: Configuration =>
      JObject(conf.parameters.toList.map {
        case (key, value) =>
          def readParameter[V](param: Parameter[V]): JValue = {
            param.toJson(value.asInstanceOf[V])
          }

          JField(objectToName(key), readParameter(key))
      })
  }
    ))

  def nameToObject[T](name: String): T = {
    Class.forName(name).getField("MODULE$").get(null).asInstanceOf[T]
  }

  def objectToName(o: Any) = {
    o.getClass.getName
  }

}

case class ResultPool(results: Map[Configuration, Results] = Map[Configuration, Results]()) {

  def update(k: Configuration, v: Results) = ResultPool(results + (k -> v))

  def buildColumns[X](analysis: Analysis, rows: Parameter[X]): List[Configuration] =
    Analysis.generateConfigurations(analysis,
      analysis.parameters.filter {
        case `rows` => false
        case _ => true
      })

  def extractData[X](analysis: Analysis,
                     rows: Parameter[X],
                     columns: List[Configuration],
                     f: Results => Double): List[(X, List[Double])] = {

    for (row <- analysis(rows))
    yield (row, columns.map { c => f(results(c.set(rows, row)))})
  }

}
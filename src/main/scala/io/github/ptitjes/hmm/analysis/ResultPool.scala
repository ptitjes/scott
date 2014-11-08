package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.{Parameter, Configuration}
import org.json4s.JsonAST.{JField, JObject, JArray}
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization._

import scala.collection.Map

case class ResultPool(results: Map[Configuration, Results] = Map[Configuration, Results]()) {

  def update(k: Configuration, v: Results) = ResultPool(results + (k -> v))

  def buildColumns[X](configurations: ConfigurationSet, rows: Parameter[X]): List[Configuration] =
    configurations.generate(Set(rows))

  def extractData[X](configurations: ConfigurationSet,
                     rows: Parameter[X],
                     columns: List[Configuration],
                     f: Results => Double): List[(X, List[Double])] = {

    for (row <- configurations(rows))
    yield (row, columns.map { c =>
      val conf = Analysis.completeConfiguration(c.set(rows, row))
      f(results(conf))
    })
  }
}

object ResultPool {

  def loadResults(file: File): ResultPool = {
    using(new FileReader(file)) {
      fileReader => using(new BufferedReader(fileReader)) {
        reader => read[ResultPool](reader)
      }
    }
  }

  def saveResults(file: File, results: ResultPool): Unit = {
    if (!file.getParentFile.exists()) file.getParentFile.mkdirs()

    using(new FileWriter(file)) {
      fileOutput => using(new PrintWriter(fileOutput)) {
        out => writePretty(results, out)
      }
    }
  }

  def nameToObject[T](name: String): T = {
    Class.forName(name).getField("MODULE$").get(null).asInstanceOf[T]
  }

  def objectToName(o: Any) = {
    o.getClass.getName
  }

  implicit val formats = Serialization.formats(NoTypeHints) +
    new ResultPoolSerializer +
    new ConfigurationSerializer

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

}
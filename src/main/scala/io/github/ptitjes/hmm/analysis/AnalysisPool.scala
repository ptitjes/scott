package io.github.ptitjes.hmm.analysis

import java.io._

import io.github.ptitjes.hmm.HiddenMarkovModel._
import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm.{Configuration, HiddenMarkovModel, Parameter}
import org.json4s.JsonAST.{JArray, JField, JObject}
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization._

import scala.collection._
import scala.collection.immutable.::

class AnalysisPool(val cacheFile: File) {

	val trainingData = mutable.Map[Configuration, TrainingData]()
	val decodingData = mutable.Map[(Configuration, Configuration), DecodingData]()

	val hmmCache = mutable.Map[Configuration, HiddenMarkovModel]()
	val resultsCache = mutable.Map[(Configuration, Configuration), Results]()

	implicit val formats = AnalysisPool.formats

	loadData()

	def hasTraining(conf: Configuration): Boolean = {
		trainingData.contains(conf)
	}

	def hasDecoding(confs: (Configuration, Configuration)): Boolean = {
		decodingData.contains(confs)
	}

	def addTraining(conf: Configuration, hmm: HiddenMarkovModel, elapsedTime: Long) = {
		val filename = "analysis/hmms/" + conf.toFilename + ".json"
		toFile(hmm, new File(filename))

		hmmCache += conf -> hmm
		trainingData += conf -> TrainingData(filename, elapsedTime)
		saveData()
	}

	def addDecoding(confs: (Configuration, Configuration), results: Results, elapsedTime: Long) = {
		val filename = resultsFilename(confs)
		saveResults(results, filename)

		resultsCache += confs -> results
		decodingData += confs -> DecodingData(filename, elapsedTime)
		saveData()
	}

	def getTraining(conf: Configuration): (HiddenMarkovModel, Long) = {
		if (!hasTraining(conf)) throw new IllegalArgumentException

		val data = trainingData(conf)
		val hmm =
			if (hmmCache.contains(conf)) hmmCache(conf)
			else loadTraining(conf, data)


		(hmm, data.elapsedTime)
	}

	def loadTraining(conf: Configuration, data: TrainingData): HiddenMarkovModel = {
		val loaded = fromFile(new File(data.hmmFilename))
		hmmCache(conf) = loaded
		loaded
	}

	def getDecoding(confs: (Configuration, Configuration)): (Results, Long) = {
		if (!hasDecoding(confs)) throw new IllegalArgumentException

		val data = decodingData(confs)
		val results =
			if (resultsCache.contains(confs)) resultsCache(confs)
			else loadDecoding(confs, data)

		(results, data.elapsedTime)
	}

	def loadDecoding(confs: (Configuration, Configuration), data: DecodingData): Results = {
		val loaded = loadResults(data.resultFilename)
		resultsCache(confs) = loaded
		loaded
	}

	def buildColumns(configurations: ConfigurationSet, rows: Parameter[_]): List[Configuration] =
		configurations.generate(Set(rows))

	def extractData[X](configurations: ConfigurationSet,
	                   rows: Parameter[X],
	                   columns: List[Configuration],
	                   f: Results => Double): List[(X, List[Double])] = {

		for (row <- configurations(rows))
		yield (row, columns.map { c =>
			val conf = c.set(rows, row)
			val trainingConf = conf.completeForTraining
			val decodingConf = conf.completeForDecoding
			val (results, _) = getDecoding((trainingConf, decodingConf))
			f(results)
		})
	}

	private def loadData(): Unit = {
		if (cacheFile.exists()) {
			val analysisData = using(new FileReader(cacheFile)) {
				fileReader => using(new BufferedReader(fileReader)) {
					reader => read[AnalysisData](reader)
				}
			}
			trainingData.clear()
			trainingData ++= analysisData.training
			decodingData.clear()
			decodingData ++= analysisData.decoding
		}
	}

	private def saveData(): Unit = {
		if (!cacheFile.getParentFile.exists()) cacheFile.getParentFile.mkdirs()

		using(new FileWriter(cacheFile)) {
			fileOutput => using(new PrintWriter(fileOutput)) {
				out => writePretty(AnalysisData(trainingData, decodingData), out)
			}
		}
	}

	private def resultsFilename(confs: (Configuration, Configuration)): String = {
		"analysis/decodings/" + confs._1.toFilename + "-with-" + confs._2.toFilename + ".json"
	}

	private def loadResults(resultsFilename: String): Results = {
		val resultFile = new File(resultsFilename)
		using(new FileReader(resultFile)) {
			fileReader => using(new BufferedReader(fileReader)) {
				reader => read[Results](reader)
			}
		}
	}

	private def saveResults(results: Results, resultsFilename: String): Unit = {
		val resultFile = new File(resultsFilename)
		if (!resultFile.getParentFile.exists()) resultFile.getParentFile.mkdirs()

		using(new FileWriter(resultFile)) {
			fileOutput => using(new PrintWriter(fileOutput)) {
				out => writePretty(results, out)
			}
		}
	}
}

case class AnalysisData(training: Map[Configuration, TrainingData],
                        decoding: Map[(Configuration, Configuration), DecodingData])

case class TrainingData(hmmFilename: String, elapsedTime: Long)

case class DecodingData(resultFilename: String, elapsedTime: Long)

object AnalysisPool {

	def nameToObject[T](name: String): T = {
		Class.forName(name).getField("MODULE$").get(null).asInstanceOf[T]
	}

	def objectToName(o: Any) = {
		o.getClass.getName
	}

	implicit val formats = Serialization.formats(NoTypeHints) +
		new ConfigurationSerializer + new AnalysisDataSerializer + FieldSerializer[ErrorCount]()

	class AnalysisDataSerializer extends CustomSerializer[AnalysisData](format => ( {
		case JObject(JField("training", JArray(training)) :: JField("decoding", JArray(decoding)) :: Nil) =>
			val trainingData = training.foldLeft(Map[Configuration, TrainingData]()) {
				case (map, JObject(JField("configuration", conf) :: JField("data", data) :: Nil)) =>
					map + (Extraction.extract[Configuration](conf) -> Extraction.extract[TrainingData](data))
			}
			val decodingData = decoding.foldLeft(Map[(Configuration, Configuration), DecodingData]()) {
				case (map, JObject(JField("configurations", confs) :: JField("data", data) :: Nil)) =>
					map + (Extraction.extract[(Configuration, Configuration)](confs) -> Extraction.extract[DecodingData](data))
			}
			AnalysisData(trainingData, decodingData)
	}, {
		case analysisData: AnalysisData =>
			JObject(JField("training", JArray(
				analysisData.training.toList.map {
					case (conf, data) =>
						JObject(JField("configuration", Extraction.decompose(conf)) ::
							JField("data", Extraction.decompose(data)) :: Nil)
				}
			)) :: JField("decoding", JArray(
				analysisData.decoding.toList.map {
					case (confs, data) =>
						JObject(JField("configurations", Extraction.decompose(confs)) ::
							JField("data", Extraction.decompose(data)) :: Nil)
				}
			)) :: Nil)
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
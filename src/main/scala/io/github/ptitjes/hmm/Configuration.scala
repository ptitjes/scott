package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.analysis.ResultPool._
import io.github.ptitjes.hmm.decoders.FullDecoder
import io.github.ptitjes.hmm.trainers.RelFreqTrainer
import org.json4s.JsonAST.{JBool, JInt, JString, JValue}

case class Configuration(parameters: Map[Parameter[_], Any] = Map()) {

	def set[V](parameter: Parameter[V], value: V): Configuration = Configuration(parameters + (parameter -> value))

	def copyFrom[V](param: Parameter[V], other: Configuration): Configuration = set(param, other(param))

	def merge(other: Configuration): Configuration = {
		other.parameters.keys.foldLeft(this) {
			case (c, param) => c.copyFrom(param, other)
		}
	}

	def apply[V](parameter: Parameter[V]): V =
		if (parameters.contains(parameter)) parameters(parameter).asInstanceOf[V]
		else parameter.default(this)

	override def toString =
		parameters.keys.toList.sortBy(_.name).map {
			p => (if (p.name != "") p.name + " " else "") + p.formatValue(this)
		}.mkString("; ")

	def toFilename = toString.replace(";", "").replace(" ", "-")
}

object Configuration {

	object CORPUS_RATIO extends IntParameter("Corpus Ratio", 100)

	object TRAINER extends TrainerParameter("", c => RelFreqTrainer)

	object DECODER extends DecoderParameter("", c => FullDecoder)

	def complete(configuration: Configuration): Configuration = {

		val ta = configuration(TRAINER)
		val da = configuration(DECODER)

		(List(CORPUS_RATIO) ++ ta.parameters ++ da.parameters).foldLeft(configuration) {
			case (c, param) => if (c.parameters.contains(param)) c else c.set(param, param.default(c))
		}
	}
}

trait Parameter[V] {

	def name: String

	def default: Configuration => V

	def formatValue(value: V): String

	def formatValue(c: Configuration): String = formatValue(c(this))

	def fromJson(value: JValue): V

	def toJson(value: V): JValue
}

case class BooleanParameter(name: String, default: Configuration => Boolean) extends Parameter[Boolean] {

	def this(name: String, default: Boolean) = this(name, conf => default)

	def formatValue(value: Boolean): String = if (value) "Yes" else "No"

	def fromJson(value: JValue): Boolean = value match {
		case JBool(b) => b
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: Boolean): JValue = JBool(value)
}

case class IntParameter(name: String, default: Configuration => Int) extends Parameter[Int] {

	def this(name: String, default: Int) = this(name, conf => default)

	def formatValue(value: Int): String = value.toString

	def fromJson(value: JValue): Int = value match {
		case JInt(i) => i.toInt
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: Int): JValue = JInt(value)
}

case class TrainerParameter(name: String, default: Configuration => Trainer.Factory)
	extends Parameter[Trainer.Factory]() {

	def formatValue(value: Trainer.Factory): String = value.name

	def fromJson(value: JValue): Trainer.Factory = value match {
		case JString(t) => nameToObject[Trainer.Factory](t)
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: Trainer.Factory): JValue = JString(objectToName(value))
}

case class DecoderParameter(name: String, default: Configuration => Decoder.Factory)
	extends Parameter[Decoder.Factory]() {

	def formatValue(value: Decoder.Factory): String = value.name

	def fromJson(value: JValue): Decoder.Factory = value match {
		case JString(t) => nameToObject[Decoder.Factory](t)
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: Decoder.Factory): JValue = JString(objectToName(value))
}

package io.github.ptitjes.hmm

import io.github.ptitjes.hmm.analysis.AnalysisPool._
import io.github.ptitjes.hmm.decoders.FullDecoder
import io.github.ptitjes.hmm.trainers.RelFreqTrainer
import org.json4s.JsonAST.{JBool, JInt, JString, JValue}

case class Configuration(parameters: Map[Parameter[_], Any] = Map()) {

	def set[V](parameter: Parameter[V], value: V): Configuration = Configuration(parameters + (parameter -> value))

	def copyFrom[V](param: Parameter[V], other: Configuration): Configuration = set(param, other(param))

	def +(other: Configuration): Configuration = {
		other.parameters.keys.foldLeft(this) {
			case (c, param) => c.copyFrom(param, other)
		}
	}

	def apply[V](parameter: Parameter[V]): V =
		if (parameters.contains(parameter)) parameters(parameter).asInstanceOf[V]
		else parameter.default(this)

	def get[V](parameter: Parameter[V]): Option[V] =
		if (parameters.contains(parameter)) Some(parameters(parameter).asInstanceOf[V])
		else None

	import Configuration._

	def completeForTraining: Configuration = {
		val ta = this(TRAINER)
		(List(TRAINER, CORPUS_RATIO) ++ ta.parameters).foldLeft(Configuration()) {
			case (c, param) => c.set(param, this(param))
		}
	}

	def completeForDecoding: Configuration = {
		val da = this(DECODER)
		(List(DECODER) ++ da.parameters).foldLeft(Configuration()) {
			case (c, param) => c.set(param, this(param))
		}
	}

	override def toString =
		(get(TRAINER).map(v => TRAINER.format(v)).toList ++
			get(DECODER).map(v => DECODER.format(v)).toList ++
			(parameters.keySet -(TRAINER, DECODER)).toList
				.sortBy(_.name).map {
				p => (if (p.name != "") p.name + "=" else "") + p.format(this)
			}).mkString(" ")

	def toFilename = toString.replace(" ", "-")
}

object Configuration {

	object CORPUS_RATIO extends IntParameter("Corpus Ratio", 100)

	object TRAINER extends ScalaObjectParameter[Trainer.Factory]("", c => RelFreqTrainer) {

		def format(value: Trainer.Factory): String = value.name
	}

	object DECODER extends ScalaObjectParameter[Decoder.Factory]("", c => FullDecoder) {

		def format(value: Decoder.Factory): String = value.name
	}

}

trait Parameter[V] {

	def name: String

	def default: Configuration => V

	def format(value: V): String

	def format(c: Configuration): String = format(c(this))

	def fromJson(value: JValue): V

	def toJson(value: V): JValue
}

case class BooleanParameter(name: String, default: Configuration => Boolean) extends Parameter[Boolean] {

	def this(name: String, default: Boolean) = this(name, conf => default)

	def format(value: Boolean): String = if (value) "Yes" else "No"

	def fromJson(value: JValue): Boolean = value match {
		case JBool(b) => b
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: Boolean): JValue = JBool(value)
}

case class IntParameter(name: String, default: Configuration => Int) extends Parameter[Int] {

	def this(name: String, default: Int) = this(name, conf => default)

	def format(value: Int): String = value.toString

	def fromJson(value: JValue): Int = value match {
		case JInt(i) => i.toInt
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: Int): JValue = JInt(value)
}

case class StringParameter(name: String, default: Configuration => String) extends Parameter[String] {

	def this(name: String, default: String) = this(name, conf => default)

	def format(value: String): String = value

	def fromJson(value: JValue): String = value match {
		case JString(v) => v
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: String): JValue = JString(value)
}

abstract case class ScalaObjectParameter[T](name: String, default: Configuration => T)
	extends Parameter[T]() {

	def fromJson(value: JValue): T = value match {
		case JString(t) => nameToObject[T](t)
		case _ => throw new IllegalArgumentException
	}

	def toJson(value: T): JValue = JString(objectToName(value))
}

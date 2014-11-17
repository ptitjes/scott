package io.github.ptitjes.hmm

import org.json4s.JsonAST.{JBool, JInt, JValue}

trait Algorithm[T] {

	def name: String

	def parameters: Set[Parameter[_]] = Set()

	def instantiate(configuration: Configuration): T
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

package io.github.ptitjes.hmm

import org.json4s.JsonAST.{JInt, JValue}

trait Algorithm[T] {

  def name: String

  def parameters: Set[Parameter[_]] = Set()

  def instantiate(configuration: Configuration): T
}

trait Parameter[V] {

  def name: String

  def default: V

  def formatValue(value: V): String

  def formatValue(c: Configuration): String = formatValue(c(this))

  def fromJson(value: JValue): V

  def toJson(value: V): JValue
}

case class IntParameter(name: String, default: Int) extends Parameter[Int] {

  def formatValue(value: Int): String = value.toString

  def fromJson(value: JValue): Int = value match {
    case JInt(i) => i.toInt
    case _ => throw new IllegalArgumentException
  }

  def toJson(value: Int): JValue = JInt(value)
}

case class Configuration(parameters: Map[Parameter[_], Any] = Map()) {

  def set[V](parameter: Parameter[V], value: V): Configuration = Configuration(parameters + (parameter -> value))

  def apply[V](parameter: Parameter[V]): V =
    if (parameters.contains(parameter)) parameters(parameter).asInstanceOf[V]
    else parameter.default

  override def toString = parameters.keys.map(p => s"${p.name} ${p.formatValue(this)}").mkString("; ")
}

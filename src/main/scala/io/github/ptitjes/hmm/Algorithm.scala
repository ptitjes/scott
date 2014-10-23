package io.github.ptitjes.hmm

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
}

case class SimpleParameter[V](name: String, default: V) extends Parameter[V] {

  def formatValue(value: V): String = value.toString
}

case class Configuration(parameters: Map[Parameter[_], Any] = Map()) {

  def set[V](parameter: Parameter[V], value: V): Configuration = Configuration(parameters + (parameter -> value))

  def apply[V](parameter: Parameter[V]): V =
    if (parameters.contains(parameter)) parameters(parameter).asInstanceOf[V]
    else parameter.default

  override def toString = parameters.keys.map(p => s"${p.name} ${p.formatValue(this)}").mkString("; ")
}

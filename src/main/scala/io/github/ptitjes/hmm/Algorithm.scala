package io.github.ptitjes.hmm

trait Algorithm[T] {

  def name: String

  def parameters: Set[Parameter[_]] = Set()

  def instantiate(configuration: Configuration): T
}

case class Parameter[V](name: String, default: V)

case class Configuration(parameters: Map[Parameter[_], Any] = Map()) {

  def set[V](parameter: Parameter[V], value: V): Configuration = Configuration(parameters + (parameter -> value))

  def apply[V](parameter: Parameter[V]): V =
    if (parameters.contains(parameter)) parameters(parameter).asInstanceOf[V]
    else parameter.default
}

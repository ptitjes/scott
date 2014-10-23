package io.github.ptitjes.hmm

trait Algorithm[T] {

  def name: String

  def parameters: Set[Parameter[_]] = Set()

  def instantiate(configuration: Configuration): T
}

case class Parameter[V](name: String)

case class Configuration(parameters: Map[Parameter[_], Any] = Map()) {

  def set[V](parameter: Parameter[V], value: V): Configuration = Configuration(parameters + (parameter -> value))

  def apply[V](parameter: Parameter[V]): V = parameters(parameter).asInstanceOf[V]
}

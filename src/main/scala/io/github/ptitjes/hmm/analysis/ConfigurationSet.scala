package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._

trait ConfigurationSet {

	def *(rhs: ConfigurationSet) = ConfSetProduct(this, rhs)

	def +(rhs: ConfigurationSet) = ConfSetAddition(this, rhs)

	def generate(): List[Configuration] = generate(Set())

	def generate(excluded: Set[Parameter[_]]): List[Configuration]

	def apply[U](parameter: Parameter[U]): List[U]
}

case class ConfSetSingle[V](param: Parameter[V], values: List[V]) extends ConfigurationSet {

	def generate(excluded: Set[Parameter[_]]): List[Configuration] = {
		if (excluded.contains(param)) List(Configuration())
		else values.map(v => Configuration().set(param, v))
	}

	def apply[U](parameter: Parameter[U]): List[U] =
		if (parameter == param) values.asInstanceOf[List[U]] else List()
}

case class ConfSetAddition(left: ConfigurationSet, right: ConfigurationSet) extends ConfigurationSet {

	def generate(excluded: Set[Parameter[_]]): List[Configuration] = {
		left.generate(excluded) ++ right.generate(excluded)
	}

	def apply[U](parameter: Parameter[U]): List[U] = left(parameter) ++ right(parameter)
}

case class ConfSetProduct(left: ConfigurationSet, right: ConfigurationSet) extends ConfigurationSet {

	def generate(excluded: Set[Parameter[_]]): List[Configuration] = {
		val leftConfigurations = left.generate(excluded)
		val rightConfigurations = right.generate(excluded)
		leftConfigurations.flatMap(l => rightConfigurations.map(r => l.merge(r)))
	}

	def apply[U](parameter: Parameter[U]): List[U] = left(parameter) ++ right(parameter)
}

object ConfigurationSet {

	implicit class RichParameter[V](param: Parameter[V]) {
		def as(value: V) = ConfSetSingle(param, List(value))

		def forAll(value: V) = new RichParameterContinuation(param, List(value))
	}

	class RichParameterContinuation[V](param: Parameter[V], values: List[V]) {
		def and(value: V) = new RichParameterContinuation(param, values ++ List(value))

		def make() = ConfSetSingle(param, values.toList)
	}

	implicit def continuationToConfSet(cont: RichParameterContinuation[_]) = cont.make()

	implicit class RichIntParameter(param: Parameter[Int]) {
		def from(values: Range) = ConfSetSingle(param, values.toList)
	}

}

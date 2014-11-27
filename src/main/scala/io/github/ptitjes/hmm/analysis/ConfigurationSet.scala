package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._

import scala.collection._

trait ConfigurationSet {

	def *(rhs: Configuration): ConfigurationSet = {
		def makeConfSet[X](p: Parameter[X]): ConfigurationSet = {
			ValuedParameter[X](p, rhs(p))
		}
		ConfSetProduct(this,
			rhs.parameters.map {
				case (p, _) => makeConfSet(p)
			}.reduce((a, b) => ConfSetProduct(a, b))
		)
	}

	def *(rhs: ConfigurationSet) = ConfSetProduct(this, rhs)

	def +(rhs: ConfigurationSet) = ConfSetAddition(this, rhs)

	def generate(): List[Configuration] = generateWithout(Set[Parameter[_]]())

	def generateWithout(excluded: Set[Parameter[_]]): List[Configuration]

	def flatten() = generate().head

	def apply[U](parameter: Parameter[U]): List[U]
}

object ConfSetEmpty extends ConfigurationSet {

	def generateWithout(excluded: Set[Parameter[_]]): List[Configuration] = {
		List(Configuration())
	}

	def apply[U](parameter: Parameter[U]): List[U] = List()
}

case class ValuedParameter[V](parameter: Parameter[V], value: V) extends ConfigurationSet {

	def generateWithout(excluded: Set[Parameter[_]]): List[Configuration] = {
		if (excluded.contains(parameter)) List(Configuration())
		else List(Configuration().set(parameter, value))
	}

	def apply[U](parameter: Parameter[U]): List[U] =
		if (parameter == parameter) List(value.asInstanceOf[U]) else List()
}

case class MultiValuedParameter[V](parameter: Parameter[V], values: List[V]) extends ConfigurationSet {

	def generateWithout(excluded: Set[Parameter[_]]): List[Configuration] = {
		if (excluded.contains(parameter)) List(Configuration())
		else values.map(v => Configuration().set(parameter, v))
	}

	def apply[U](parameter: Parameter[U]): List[U] =
		if (parameter == parameter) values.asInstanceOf[List[U]] else List()
}

case class ConfSetAddition(left: ConfigurationSet, right: ConfigurationSet) extends ConfigurationSet {

	def generateWithout(excluded: Set[Parameter[_]]): List[Configuration] = {
		left.generateWithout(excluded) ++ right.generateWithout(excluded)
	}

	def apply[U](parameter: Parameter[U]): List[U] = left(parameter) ++ right(parameter)
}

case class ConfSetProduct(left: ConfigurationSet, right: ConfigurationSet) extends ConfigurationSet {

	def generateWithout(excluded: Set[Parameter[_]]): List[Configuration] = {
		val leftConfigurations = left.generateWithout(excluded)
		val rightConfigurations = right.generateWithout(excluded)
		leftConfigurations.flatMap(l => rightConfigurations.map(r => l + r))
	}

	def apply[U](parameter: Parameter[U]): List[U] = left(parameter) ++ right(parameter)
}

object ConfigurationSet {

	def apply() = ConfSetEmpty

	implicit class RichParameter[V](param: Parameter[V]) {
		def as(value: V) = ValuedParameter(param, value)

		def forAll(value: V) = new RichParameterContinuation(param, List(value))
	}

	class RichParameterContinuation[V](param: Parameter[V], values: List[V]) {
		def and(value: V) = new RichParameterContinuation(param, values ++ List(value))

		def make() = MultiValuedParameter(param, values.toList)
	}

	implicit def continuationToConfSet(cont: RichParameterContinuation[_]) = cont.make()

	implicit class RichIntParameter(param: Parameter[Int]) {
		def from(values: Range) = MultiValuedParameter(param, values.toList)
	}

}

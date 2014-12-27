package io.github.ptitjes.scott.api

/**
 * @author Didier Villevalois
 */
trait Trainer[X, Y <: X] {

	def train(corpus: DataSet[Y]): HiddenMarkovModel[X, Y]
}

package io.github.ptitjes.scott.api

/**
 * @author Didier Villevalois
 */
trait IterationCallback[X, Y <: X] {

	def iterationDone(iteration: Int, hmm: HiddenMarkovModel[X, Y], elapsedTime: Long): Unit
}

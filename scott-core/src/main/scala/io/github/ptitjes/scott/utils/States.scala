package io.github.ptitjes.scott.utils

import io.github.ptitjes.scott.Utils._

/**
 * @author Didier Villevalois
 */
object States {

	def stateToTagSeq(breadth: Int, order: Int, depth: Int, sourceState: Int) = {
		def tagList(i: Int, state: Int): List[Int] = {
			if (i == 0) Nil
			else if (i <= order - depth) -1 :: tagList(i - 1, state / breadth)
			else {
				(state % breadth) :: tagList(i - 1, state / breadth)
			}
		}
		tagList(order, sourceState).toIndexedSeq
	}

	class SourceTracker(val breadth: Int, val order: Int) {

		private val size = pow(breadth, order)

		private var _currentDepth = 0
		private var _index = -1
		private var _sourceState = -1

		def reset(): Unit = {
			_currentDepth = 0
			_index = -1
			_sourceState = -1
		}

		def append(tag: Int) = {
			_index += 1

			if (_index == 0)
				_sourceState = tag
			else if (_index > 0)
				_sourceState = (_sourceState * breadth + tag) % size

			if (_currentDepth < order) {
				_currentDepth += 1
			}
		}

		def state: Int = {
			_sourceState
		}

		def tags: IndexedSeq[Int] = stateToTagSeq(breadth, order, _currentDepth, _sourceState)
	}

}

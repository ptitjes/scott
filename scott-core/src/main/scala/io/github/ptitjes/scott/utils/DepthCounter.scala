package io.github.ptitjes.scott.utils

/**
 * @author Didier Villevalois
 */
class DepthCounter(val maxDepth: Int) {

	private var _currentDepth: Int = 0

	def reset(): Unit = {
		_currentDepth = 0
	}

	def next(): Unit = {
		if (_currentDepth < maxDepth) {
			_currentDepth += 1
		}
	}

	def current: Int = {
		_currentDepth
	}
}

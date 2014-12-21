package io.github.ptitjes.scott.decoders

import scala.reflect.ClassTag

/**
 * @author Didier Villevalois
 */
object arrays {

	final class SwappableArray[T: ClassTag](size: Int) {
		private var current: Array[T] = new Array[T](size)
		private var last: Array[T] = new Array[T](size)

		def apply(i: Int): T = last(i)

		def update(i: Int, v: T) = current(i) = v

		def swap(): Unit = {
			val temp = current
			current = last
			last = temp
		}
	}

	final class PsiArray(size: Int, length: Int) {
		private val data = Array.ofDim[Int](length, size)
		private var index = 0

		def apply(i: Int): Int = data(index - 1)(i)

		def update(i: Int, v: Int) = data(index)(i) = v

		def forward(): Unit = index = index + 1

		def backward(): Unit = index = index - 1

		def isRewound: Boolean = index == 0

		def rewind(): Unit = index = 0
	}

}

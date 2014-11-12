package io.github.ptitjes.hmm

object Utils {

	def log(v: Int): Double = math.log(v)

	//	def pow(a: Int, b: Int): Int = {
	//		var i = b
	//		var p = 1
	//		while (i != 0) {
	//			p *= a
	//			i -= 1
	//		}
	//		p
	//	}

	def pow(x: Int, n: Int): Int = {
		if (n == 0) 1
		else if ((n & 1) == 0) {
			val y = pow(x, n >> 1)
			y * y
		} else x * pow(x, n - 1)
	}

	// def pow(a: Int, b: Int): Int = math.pow(a, b).asInstanceOf[Int]

	def avoidInfinity(logProbability: Double) =
		if (logProbability.isNegInfinity || logProbability.isNaN) -1.0e307 else logProbability

	def timed[T](step: String)(execution: => T): T = {
		val start = System.currentTimeMillis()
		val result = execution
		val time = System.currentTimeMillis() - start
		println(step + ": " + time + "ms")
		result
	}

	def timed[T](execution: => T): (T, Long) = {
		val start = System.currentTimeMillis()
		val result = execution
		val time = System.currentTimeMillis() - start
		(result, time)
	}

	def using[A <: AutoCloseable, B](param: A)(f: A => B): B =
		try {
			f(param)
		} finally {
			param.close()
		}
}

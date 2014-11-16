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

	def prettyTime(time: Long): String = {
		val inSeconds = math.ceil(time.toDouble / 1000).toInt
		val s = inSeconds % 60
		val m = inSeconds / 60 % 60
		val h = inSeconds / 3600

		f"$h%02d:$m%02d:$s%02d"
	}

	class ProgressBar(name: String, count: Int) {
		var done = 0

		val start = System.currentTimeMillis()

		def increment(): Unit = {
			this.done += 1
			update()
		}

		def set(done: Int): Unit = {
			this.done = done
			update()
		}

		def update(): Unit = {
			val time = System.currentTimeMillis() - start

			if (done < count) {
				val remainingTime =
					if (done < count / 10) "??"
					else prettyTime(time * count / done - time)

				val doneSize = done * 100 / count
				print(f"$name: $done%5d/$count |" + "=" * doneSize + " " * (100 - doneSize) +
					"| Remaining: " + remainingTime + "\r")
			} else {
				val totalTime = prettyTime(time)

				print(f"$name: $done%5d/$count |" + "=" * 100 +
					"|     Total: " + totalTime + "\n")
			}
		}
	}

}

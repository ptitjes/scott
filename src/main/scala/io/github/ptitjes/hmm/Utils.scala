package io.github.ptitjes.hmm

object Utils {

	def log(v: Int): Double = math.log(v)

	def pow(x: Int, n: Int): Int = {
		if (n == 0) 1
		else if ((n & 1) == 0) {
			val y = pow(x, n >> 1)
			y * y
		} else x * pow(x, n - 1)
	}

	def avoidInfinity(logProbability: Double) =
		if (logProbability.isNegInfinity || logProbability.isNaN) -1.0e307 else logProbability

	def timed[T](step: String)(execution: => T): (T, Long) = {
		val indicator = new ProgressIndicator(step)

		try {
			val start = System.currentTimeMillis()
			val result = execution
			val time = System.currentTimeMillis() - start

			indicator.stop(time)
			(result, time)
		} finally {
			indicator.stop()
		}
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

	def prettyTimeMs(time: Long): String = {
		val inSeconds = time / 1000
		val ms = time % 1000
		val s = inSeconds % 60
		val m = inSeconds / 60 % 60
		val h = inSeconds / 3600

		f"$h%02d:$m%02d:$s%02d.$ms%03d"
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
				val totalTime = prettyTimeMs(time)

				print(f"$name: $done%5d/$count |" + "=" * 100 +
					"|     Total: " + totalTime + "\n")
			}
		}
	}

	class ProgressIndicator(val name: String) {

		var finished = false

		private val thread = new Thread(new Runnable {
			val chars = "/-\\|".toCharArray

			override def run(): Unit = {
				var i = 0
				while (!finished) {
					print(name + ": " + chars(i) + "\r")
					Thread.sleep(500)
					i = (i + 1) % chars.length
				}
			}
		})
		thread.start()

		def stop(): Unit = {
			finished = true
			thread.join()
		}

		def stop(time: Long): Unit = {
			finished = true
			thread.join()
			print(name + ": " + prettyTimeMs(time) + "\n")
		}
	}

}

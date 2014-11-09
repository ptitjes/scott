package io.github.ptitjes.hmm

object Utils {

  def log(v: Int) = math.log(v)

  def pow(a: Int, b: Int) = math.pow(a, b).asInstanceOf[Int]

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

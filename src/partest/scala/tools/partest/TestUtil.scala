package scala.tools.partest

trait TestUtil {
  /** Given function and block of code, evaluates code block,
   *  calls function with nanoseconds elapsed, and returns block result.
   */
  def timed[T](f: Long => Unit)(body: => T): T = {
    val start = System.nanoTime
    val result = body
    val end = System.nanoTime

    f(end - start)
    result
  }
  /** Times body and returns (nanos, result).
   */
  def alsoNanos[T](body: => T): (Long, T) = {
    var nanos = 0L
    val result = timed(nanos = _)(body)

    (nanos, result)
  }
  def nanos(body: => Unit): Long = alsoNanos(body)._1

  def verifySpeed(body1: => Unit, body2: => Unit, acceptableMultiple: Double) = {
    val t1 = nanos(body1).toDouble
    val t2 = nanos(body2).toDouble
    val mult = if (t1 > t2) t1 / t2 else t2 / t1

    assert(mult <= acceptableMultiple, "Performance difference too great: multiple = " + mult)
  }
}

object TestUtil extends TestUtil {

}
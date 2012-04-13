package scala.concurrent.util.duration

import scala.concurrent.util.{ Duration }

/*
 * Avoid reflection based invocation by using non-duck type
 */
protected[duration] class IntMult(i: Int) {
  def *(d: Duration) = d * i
}

protected[duration] class LongMult(i: Long) {
  def *(d: Duration) = d * i
}

protected[duration] class DoubleMult(f: Double) {
  def *(d: Duration) = d * f
}

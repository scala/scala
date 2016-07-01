package p

import scala.language.future.noStringPlus

// Because the argument is also a string, need to opt out of any2stringadd too
import scala.Predef.{ any2stringadd => _, _ }

object Test {
  def a(s1: String, s2: String) = s1 + s2
}

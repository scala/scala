package p

import scala.language.future.noStringPlus

// Because one argument is also a string, need to opt out of any2stringadd too
import scala.Predef.{ any2stringadd => _, _ }

object Test {
  implicit class StringWithPlusString(val s: String) extends AnyVal {
    def +(x: String): String = s"$s$x"
  }

  def a(s1: String, s2: String) = s1 + s2
  def b(s1: String, b: Boolean) = s1 + b.toString
}

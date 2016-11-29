package p

// Because one argument is also a string, need to opt out of any2stringadd too
import Predef.{ any2stringadd => _, _ }

trait Show[-A] { def show(x: A): String }
object Show {
  implicit def showString: Show[String] = new Show[String] { def show(x: String) = x }
  implicit def showBoolean: Show[Boolean] = new Show[Boolean] { def show(x: Boolean) = x.toString }
}

object Test {
  implicit class StringWithPlusString(val s: String) extends AnyVal {
    def +(x: String): String = s"$s$x"
  }

  def a(s1: String, s2: String) = s1 + s2
  def b(s1: String, b: Boolean) = s1 + b.toString
}

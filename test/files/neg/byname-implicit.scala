//> using options -Werror -Xlint:byname-implicit

import language._

class C {
  implicit def `ints are strs`(n: => Int): String = n.toString
  implicit def `bools are strs`[A](b: => A)(implicit ev: A => Boolean): String = if (b) "yup" else "nah"

  def show(s: String) = println(s"[$s]")

  def f(): Unit =
    show {
      println("see me")
      42                      // warn
    }

  def g(): Unit =
    show {
      42                      // no warn
    }

  def truly(): Unit =
    show {
      println("see me")
      true                    // warn
    }

  def falsely(): Unit =
    show {
      false                   // no warn
    }
}

// the workaround for https://github.com/erikvanoosten/metrics-scala/issues/42
// This removes callsite verbosity with extra wrapping in a by-name arg, but the implicit still warns,
// because the call to healthCheck still requires the implicit conversion to Magnet.
// Here, the by-name should be removed from the implicit because by-name semantics is already provided by healthCheck.

package object health {
  type HealthCheck = () => String

  def healthCheck(name: String, unhealthyMessage: String = "Health check failed")(checker: => HealthCheckMagnet): HealthCheck = {
    () => checker(unhealthyMessage)()
  }
}

package health {

  /*sealed*/ trait HealthCheckMagnet {
    def apply(unhealthyMessage: String): HealthCheck
  }

  object HealthCheckMagnet {

    /** Magnet for checkers returning a [[scala.Boolean]] (possibly implicitly converted).  */
    implicit def fromBooleanCheck[A](checker: => A)(implicit ev: A => Boolean): HealthCheckMagnet = new HealthCheckMagnet {
      def apply(unhealthyMessage: String) = () => if (checker) "OK" else unhealthyMessage
    }
  }
}

object Test extends App {
  import health._

  var count = 0
  val res = healthCheck("bool test", "nope") {
    count += 1
    false                  // warn
  }
  assert(res() == "nope")
  assert(res() == "nope")
  assert(count == 2)
}

// t9386
class Foo
object Foo {
  implicit def int2Foo(a: => Int): Foo = new Foo //Never evaluate the argument
  def bar(foo: Foo) = ()
  def bar(foo: Boolean) = () //unrelated overload
}

object FooTest extends App {
  Foo.bar { println("barring"); 0 }  // warn
}

class Nowarn {
  implicit def cv(n: => Int): String = n.toString

  def show(s: String) = println(s"[$s]")

  def f(): Unit = show(cv(42))  // nowarn because it only warns if statements
  def g(): Unit = show { println(); cv(42) }  // nowarn anymore because explicit call by user
}

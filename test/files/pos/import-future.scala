//> using options -Xsource:3

import java.io as jio
import scala.{collection as c}

import c.mutable as mut
import mut.ArrayBuffer as Buf

object O {
  val x: jio.IOException = ???
  val y = Buf(1, 2, 3)

  type OString = String
  def foo22(x: Int) = x
}

class C {
  import O.{ foo22 as foo, OString as OS }
  println(foo(22))
  val s: OS = ""

  import mut.*
  val ab = ArrayBuffer(1)
}

object starring {

  import scala.concurrent.{*, given}, duration.{Duration as D, given, *}, ExecutionContext.Implicits.*

  val f = Future(42)
  val r = Await.result(f, D.Inf)
}

trait T[A] {
  def t: A
}
object T {
  implicit def tInt: T[Int] = new T[Int] {
    def t: Int = 42
  }
  def f[A](implicit t: T[A]): A = t.t
}
object X {
  import T.given
  def g = T.f[Int] // was given is not a member
}

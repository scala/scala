//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { assert(test().toString == "(C,C)")

  class C {
    override def toString = "C"
  }

  def foo[A <: C](a: A) = async {a}

  def test1[CC <: C](c: CC): Future[(CC, CC)] = async {
    val x: (CC, CC) = 0 match {
      case _ if false => ???;
      case _          => (await(foo(c)), await(foo(c)))
    }
    x
  }
  def test(): (C, C) = Await.result(test1(new C), Duration.Inf)
}

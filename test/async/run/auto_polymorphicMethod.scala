import scala.tools.nsc.transform.async.user.{async, autoawait}

object Test extends App { assert(test.toString == "(C,C)")

  class C {
    override def toString = "C"
  }

  @autoawait def foo[A <: C](a: A): A = a
  @async
  def test1[CC <: C](c: CC): (CC, CC) = {
    val x: (CC, CC) = 0 match {
      case _ if false => ???;
      case _          => (foo(c), foo(c))
    }
    x
  }
  def test(): (C, C) = test1(new C)
}

package tastytest

import scala.language.future
import scala.language.experimental.modularity

object TrackedParam {

  class C {
    type T
    lazy val t: T = ???
  }

  def f(x: C): x.T = x.t

  val y: C { type T = Int } = ???

  class F(tracked val x: C) {
    lazy val result: x.T = f(x)
  }

  def test: Int = new F(y).result
}


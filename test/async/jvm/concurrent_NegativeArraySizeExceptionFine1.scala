//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

case class FixedFoo(foo: Int)

class Foobar(val foo: Int, val bar: Double) {
  def getValue = async { 4.2 }
  def func(f: Any)  = async {
    new Foobar(foo = f match {
      case FixedFoo(x) => x
      case _           => 2
    },
                bar = await(getValue))
  }
}

object Test extends App { test()
  def test() = Await.result(new Foobar(0, 0).func(4), Duration.Inf)
}

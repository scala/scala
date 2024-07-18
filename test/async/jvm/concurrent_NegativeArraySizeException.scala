//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { test()
  def foo(foo: Any, bar: Any) = ()
  def getValue = async {4.2}
  def func(f: Any)  = async {
    foo(f match { case _ if "".isEmpty => 2 case x => throw new MatchError(x) }, await(getValue));
  }

  def test() = Await.result(func(4), Duration.Inf)
}

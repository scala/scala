//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { test

  trait Foo

  trait Bar

  def test: Any = Await.result(test(new C), Duration.Inf)
  def asyncBoundary: Future[String] = async { "" }
  def test(foo: Foo): Future[Foo] = async {
    foo match {
      case foo: Bar =>
        val foo2: Foo with Bar = new Foo with Bar {}
        await(asyncBoundary)
        null match {
          case _ => foo2
        }
      case other => foo
    }
  }

  class C extends Foo with Bar
}

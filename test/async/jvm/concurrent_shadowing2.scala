//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { test

  trait Base;

  trait Foo[T <: Base] {
    def func: Future[Option[Foo[T]]] = async { None }
  }

  class Sub extends Base

  trait Bar extends Foo[Sub]

  def test: Any = test(new Bar {})
  def test[T <: Base](foo: Foo[T]): Future[Foo[T]] = async {
    foo match {
      case foo: Bar =>
        val res = foo.func
        res match {
          case _ =>
        }
        foo
      case other => foo
    }
  }
  Await.result(test(new Bar {}), Duration.Inf)
}

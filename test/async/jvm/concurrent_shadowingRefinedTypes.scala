//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

trait Base

class Sub extends Base

trait Foo[T <: Base] {
  def func: Future[Option[Foo[T]]] = async { None }
}

trait Bar extends Foo[Sub]

object Test extends App { assert(test)
  def func[T <: Base](foo: Foo[T]): Future[Foo[T]] = async {
    foo match { // the whole pattern match will be wrapped with async{ }
      case foo: Bar =>
        val res = await(foo.func) // will be rewritten into: await(foo.func)
        res match {
          case Some(v) => v // this will report type mismtach
          case other => foo
        }
      case other => foo
    }
  }
  def test: Boolean = {
    val b = new Bar {};
    Await.result(func(b), Duration.Inf) == b
  }
}

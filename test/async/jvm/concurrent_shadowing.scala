// scalac: -Xasync
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.partest.async.Async._
import scala.concurrent.duration.Duration

object Test extends App { test

  trait Foo

  trait Bar extends Foo

  def boundary = async { "" }
  def test: Unit = Await.result(async {
    (new Bar {}: Any) match {
      case foo: Bar =>
        await { boundary }
        0 match {
          case _ => foo; ()
        }
        ()
    }
    ()
  }, Duration.Inf)
}

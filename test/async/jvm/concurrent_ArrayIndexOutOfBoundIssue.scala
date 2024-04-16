//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

sealed trait Result

case object A extends Result

case object B extends Result

case object C extends Result

object Test extends App { test()
  protected def doStuff(res: Result) = {
    class C {
      def needCheck = async { false }

      def m  = async {
        if (await(needCheck)) "NO"
        else {
          res match {
            case A => 1
            case _ => 2
          }
        }
      }
    }
    new C().m
  }


  def test() = Await.result(doStuff(B), Duration.Inf)
}

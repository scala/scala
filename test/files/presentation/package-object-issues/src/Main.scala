package scala.concurrent

import scala.concurrent.ExecutionException

object Main extends App {
  def foo(n: ExecutionException, k: Int): Unit = {
    n./*!*/
    k
  }
}

package tastytest

import scala.collection.mutable

import tastytest.Suite.Context
import scala.util.control.NonFatal

class Suite(val name: String) {
  val reps: Int = 1

  def test(name: String)(code: => Unit): Unit = ()

  def test(code: => Unit): Unit = ()

  def main(args: Array[String]): Unit = ()
}

object Suite {
  class Context private[Suite] (val name: String) extends AnyVal

  def context(name: String): Context = new Context(name)
}

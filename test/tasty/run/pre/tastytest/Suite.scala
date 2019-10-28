package tastytest

import scala.collection.mutable

import tastytest.Suite.Context

class Suite(val name: String) {
  private[this] val tests = mutable.ArrayBuffer.empty[(Context, Context => Unit)]

  def test(name: String)(code: Context => Unit): Unit = {
    tests += Suite.context(name) -> code
  }

  def assert(test: Boolean)(implicit ctx: Context): Unit =
    if (!test) {
      throw new AssertionError(s"in `$name.${ctx.name}`")
    }

  def main(args: Array[String]): Unit = {
    for ((ctx, test) <- tests) {
      test(ctx)
    }
    println("Suite passed!")
  }
}

object Suite {
  class Context private[Suite] (val name: String) extends AnyVal

  def context(name: String): Context = new Context(name)
}

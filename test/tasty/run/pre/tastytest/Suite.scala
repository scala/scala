package tastytest

import scala.collection.mutable

import tastytest.Suite.Context
import scala.util.control.NonFatal

class Suite(val name: String) {
  private[this] val counts = mutable.Map.empty[String, Int]
  private[this] val tests = mutable.ArrayBuffer.empty[(Context, () => Unit)]

  def reps: Int = 1

  def test(name: String)(code: => Unit): Unit = {
    val count = counts.getOrElse(name, 0)
    val name1 = if (count == 0) name else s"$name($count)"
    tests += Suite.context(name1) -> (() => code)
    counts.update(name, count + 1)
  }

  def test(code: => Unit): Unit = test("test")(code)

  def main(args: Array[String]): Unit = {
    require(reps > 0, s"reps <= 0")
    val errors = mutable.ArrayBuffer.empty[(Context, Throwable)]
    for (i <- 0 to reps) {
      runImpl(errors, i)
    }
    println("Suite passed!")
  }

  private def runImpl(errors: mutable.ArrayBuffer[(Context, Throwable)], iteration: Int): Unit = {
    for ((ctx, test) <- tests) {
      try test()
      catch {
        case NonFatal(err) => errors += (ctx -> err)
      }
    }
    if (errors.nonEmpty) {
      val msg = if (errors.size == 1) "error" else "errors"
      val msgs = errors.map {
        case (ctx, err) => s"${err.getClass.getSimpleName} in `$name.${ctx.name}`: ${err.getMessage}"
      }
      throw new AssertionError(msgs.mkString(s"${errors.size} $msg at iteration $iteration:\n", "\n", ""))
    }
  }
}

object Suite {
  class Context private[Suite] (val name: String) extends AnyVal

  def context(name: String): Context = new Context(name)
}

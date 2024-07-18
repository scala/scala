//> using options -Xasync

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

object Test {
  def main(args: Array[String]): Unit = {
     valueClass()
  }

  trait T extends Any
  class VC(val a: String) extends AnyVal with T
  def VC(a: String) = new VC(a)
  private def valueClass() = {
    val t: T = optionally[T]{
      value(Some(()))
      VC("")
    }.get
  }
}

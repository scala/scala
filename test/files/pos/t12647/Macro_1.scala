
//> using options -Xsource:3

import scala.reflect.macros.whitebox.Context

trait Result

object Macros {
  def impl(c: Context) = {
    import c.universe._
    q"""new Result { def value = "Was this the answer you sought?" }"""
  }
}

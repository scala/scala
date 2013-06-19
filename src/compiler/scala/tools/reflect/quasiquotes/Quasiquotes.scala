package scala.tools.reflect
package quasiquotes

import scala.reflect.macros.contexts.Context

abstract class Quasiquotes extends Macros
                              with Parsers
                              with Reifiers  {
  val c: Context
  val global: c.universe.type = c.universe
  import c.universe._

  def debug(msg: String): Unit =
    if (settings.Yquasiquotedebug.value) println(msg)
}

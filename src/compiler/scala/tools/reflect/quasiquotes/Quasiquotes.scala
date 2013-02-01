package scala.tools.reflect
package quasiquotes

import scala.reflect.macros.runtime.Context

abstract class Quasiquotes extends Macros
                              with Parsers
                              with Reifiers  {
  val c: Context
  val global: c.universe.type = c.universe
}

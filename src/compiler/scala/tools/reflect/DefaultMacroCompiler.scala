package scala.tools.reflect

import scala.reflect.macros.runtime.Context

abstract class DefaultMacroCompiler {
  val c: Context

  def defaultResolveMacroImpl(macroDef: c.Tree): c.Tree = {
    ???
  }
}
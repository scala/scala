package scala.reflect.makro
package runtime

class AbortMacroException(val pos: scala.tools.nsc.util.Position, val msg: String) extends Throwable(msg)

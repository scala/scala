package scala.reflect.makro
package runtime

class AbortMacroException(val pos: scala.reflect.internal.util.Position, val msg: String) extends Throwable(msg)

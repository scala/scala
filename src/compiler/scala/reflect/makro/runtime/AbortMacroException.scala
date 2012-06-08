package scala.reflect.makro
package runtime

import scala.reflect.internal.util.Position

class AbortMacroException(val pos: Position, val msg: String) extends Throwable(msg)

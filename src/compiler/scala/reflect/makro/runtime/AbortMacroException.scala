package scala.reflect.makro
package runtime

import scala.tools.nsc.util.Position

class AbortMacroException(val pos: Position, val msg: String) extends Throwable(msg)

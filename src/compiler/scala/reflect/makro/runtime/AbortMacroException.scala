package scala.reflect.makro
package runtime

import scala.reflect.api.Position

class AbortMacroException(val pos: Position, val msg: String) extends Throwable(msg)

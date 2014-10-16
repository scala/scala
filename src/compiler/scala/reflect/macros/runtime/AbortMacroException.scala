/* NSC -- new Scala compiler
 * Copyright 2012-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.macros
package runtime

import scala.reflect.internal.util.Position
import scala.util.control.ControlThrowable

class AbortMacroException(val pos: Position, val msg: String) extends Throwable(msg) with ControlThrowable
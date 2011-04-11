/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.reflect.{ Method => JMethod }

class ReplVals(final val r: ILoop) {
  final val intp       = r.intp
  final val global     = r.power.global
  final val power      = r.power
  final val phased     = r.power.phased
  final val isettings  = r.intp.isettings
  final val completion = r.in.completion
  final val history    = r.in.history
  final val rutil      = r.power.rutil
}

/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.reflect.{ Method => JMethod, Modifier => JModifier }

final class ReplVals(final val r: ILoop) {
  final val vals       = this
  final val intp       = r.intp
  final val global     = intp.global
  final val power      = r.power
  final val phased     = power.phased
  final val isettings  = intp.isettings
  final val completion = r.in.completion
  final val history    = r.in.history
  final val rutil      = power.rutil

  /** Reflectively finds the vals defined in this class. */
  private def valMethods = this.getClass.getDeclaredMethods.toList filter { m =>
    (
         JModifier.isPublic(m.getModifiers())
      && m.getParameterTypes.isEmpty
      && !m.getName.contains('$')
    )
  }

  /** Binds each val declared here into the repl with explicit singleton types
   *  based on the given prefix.
   */
  def bindWithPrefix(prefix: String) {
    valMethods foreach { m =>
      repldbg("intp.bind " + (m.getName, prefix + "." + m.getName + ".type", m.invoke(this)))
      intp.bind(m.getName, prefix + "." + m.getName + ".type", m.invoke(this))
    }
  }
}

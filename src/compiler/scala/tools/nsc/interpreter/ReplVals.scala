/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

final class ReplVals(r: ILoop) {
  lazy val repl       = r
  lazy val intp       = r.intp
  lazy val power      = r.power
  lazy val reader     = r.in
  lazy val vals       = this
  lazy val global     = intp.global
  lazy val isettings  = intp.isettings
  lazy val completion = reader.completion
  lazy val history    = reader.history
  lazy val phased     = power.phased
}

/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import scala.collection.mutable
import scala.reflect.internal.util.Position
// TODO
//import scala.reflect.internal.Reporter

/**
 * This class implements a Reporter that stores its reports in the set `infos`.
 */
class StoreReporter extends Reporter {
  case class Info(pos: Position, msg: String, severity: Severity) {
    override def toString() = s"pos: $pos $msg $severity"
  }
  val infos = new mutable.LinkedHashSet[Info]
  override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    if (!force) {
      infos += Info(pos, msg, severity)
      severity.count += 1
    }
  }
  override def reset() {
    super.reset()
    infos.clear()
  }
}

/* NSC -- new Scala compiler
 * Copyright 2002-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package reporters

import java.io.{BufferedReader, InputStreamReader, IOException, PrintWriter}

import scala.collection.mutable.HashSet
import scala.tools.nsc.util.{Position, SourceFile}

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class StoreReporter extends Reporter {
  class Info(val pos: Position, val msg: String, val severity: Severity) {
    override def toString() = "pos: " + pos + " " + msg + " " + severity
  }
  val infos = new HashSet[Info]
  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
    if (!force) {
      infos += new Info(pos, msg, severity)
      severity.count += 1
    }
  }

  override def reset() {
    super.reset
    infos.clear
  }
}

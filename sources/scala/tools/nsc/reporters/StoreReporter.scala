/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.nsc.reporters;
import scala.tools.nsc.util.Position;
import scala.collection.mutable.HashSet;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * This class implements a Reporter that displays messages on a text
 * console.
 */
class StoreReporter extends Reporter {

  class Info(val pos: Position, val msg: String, val severity: Severity) {
    override def toString() = "pos: " + pos + " " + msg + " " + severity;
  }

  val infos = new HashSet[Info];

  protected def info0(pos : Position, msg : String, severity : Severity, force : Boolean) : Unit = if (!force) {
    infos += new Info(pos, msg, severity);
    incr(severity);
  }
  override def reset = {
    super.reset;
    infos.clear;
  }
}

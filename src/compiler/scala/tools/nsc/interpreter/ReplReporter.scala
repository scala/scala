/* NSC -- new Scala compiler
 * Copyright 2002-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import reporters._
import IMain._

class ReplReporter(intp: IMain) extends ConsoleReporter(intp.settings, null, new ReplStrippingWriter(intp)) {
  override def printMessage(msg: String) {
    // Avoiding deadlock if the compiler starts logging before
    // the lazy val is complete.
    if (intp.isInitializeComplete) {
      if (intp.totalSilence) ()
      else super.printMessage(msg)
    }
    else Console.println("[init] " + msg)
  }
}

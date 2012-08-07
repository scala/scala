/* NSC -- new Scala compiler
 * Copyright 2002-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import reporters._
import IMain._

/** Like ReplGlobal, a layer for ensuring extra functionality.
 */
class ReplReporter(intp: IMain) extends ConsoleReporter(intp.settings, Console.in, new ReplStrippingWriter(intp)) {
  def printUntruncatedMessage(msg: String) = withoutTruncating(printMessage(msg))

  override def printMessage(msg: String) {
    // Avoiding deadlock if the compiler starts logging before
    // the lazy val is complete.
    if (intp.isInitializeComplete) {
      if (intp.totalSilence) {
        if (isReplTrace)
          super.printMessage("[silent] " + msg)
      }
      else super.printMessage(msg)
    }
    else Console.println("[init] " + msg)
  }

  override def displayPrompt() {
    if (intp.totalSilence) ()
    else super.displayPrompt()
  }
}

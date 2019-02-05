/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2017 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import Pretty.{Params, pretty, format}
import org.scalacheck.Test

/** A [[org.scalacheck.Test.TestCallback]] implementation that prints
 *  test results directly to the console. This is the callback used by
 *  ScalaCheck's command line test runner, and when you run
 *  `org.scalacheck.Prop.check()`.
 */
class ConsoleReporter(val verbosity: Int, val columnWidth: Int)
  extends Test.TestCallback {

  private val prettyPrms = Params(verbosity)

  override def onTestResult(name: String, res: Test.Result): Unit = {
    if(verbosity > 0) {
      if(name == "") {
        val s = (if(res.passed) "+ " else "! ") + pretty(res, prettyPrms)
        printf("\r%s\n", format(s, "", "", columnWidth))
      } else {
        val s = (if(res.passed) "+ " else "! ") + name + ": " +
          pretty(res, prettyPrms)
        printf("\r%s\n", format(s, "", "", columnWidth))
      }
    }
  }

}

object ConsoleReporter {

  /** Factory method, creates a ConsoleReporter with the
   *  the given verbosity and wraps output at the given column width
   *  (use 0 for unlimited width). */
  def apply(verbosity: Int = 0, columnWidth: Int = 75) =
    new ConsoleReporter(verbosity, columnWidth)

}

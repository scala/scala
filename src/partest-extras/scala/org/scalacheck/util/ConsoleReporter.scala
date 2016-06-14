/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import Pretty.{Params, pretty, format}
import org.scalacheck.{Prop, Properties, Test}

/** A [[org.scalacheck.Test.TestCallback]] implementation that prints
 *  test results directly to the console. This is the callback used
 *  by ScalaCheck's command line test runner, and when you run [[org.scalacheck.Prop!.check:Unit*]]
 */
class ConsoleReporter(val verbosity: Int) extends Test.TestCallback {

  private val prettyPrms = Params(verbosity)

  override def onTestResult(name: String, res: Test.Result) = {
    if(verbosity > 0) {
      if(name == "") {
        val s = (if(res.passed) "+ " else "! ") + pretty(res, prettyPrms)
        printf("\r%s\n", format(s, "", "", 75))
      } else {
        val s = (if(res.passed) "+ " else "! ") + name + ": " +
          pretty(res, prettyPrms)
        printf("\r%s\n", format(s, "", "", 75))
      }
    }
  }

}

object ConsoleReporter {

  /** Factory method, creates a ConsoleReporter with the
   *  the given verbosity */
  def apply(verbosity: Int = 0) = new ConsoleReporter(verbosity)

}

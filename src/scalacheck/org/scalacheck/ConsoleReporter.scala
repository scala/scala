/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import Pretty._
import util.FreqMap

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

  def testStatsEx(msg: String, res: Test.Result) = {
    lazy val m = if(msg.length == 0) "" else msg + ": "
    res.status match {
      case Test.Proved(_) => {}
      case Test.Passed => {}
      case f @ Test.Failed(_, _) => sys.error(m + f)
      case Test.Exhausted => {}
      case f @ Test.GenException(_) => sys.error(m + f)
      case f @ Test.PropException(_, _, _) => sys.error(m + f)
    }
  }

}

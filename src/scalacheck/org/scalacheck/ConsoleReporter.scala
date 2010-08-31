/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2010 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

import Pretty._
import util.FreqMap

class ConsoleReporter(val verbosity: Int) extends Test.TestCallback {

  private val prettyPrms = Params(verbosity)

  override def onPropEval(name: String, w: Int, s: Int, d: Int) =
    if(verbosity > 0) {
      if(name == "") {
        if(d == 0) printf("\rPassed %s tests\r", s)
        else printf("\rPassed %s tests; %s discarded\r", s, d)
      } else {
        if(d == 0) printf("\r  %s: Passed %s tests\r", name, s)
        else printf("\r  %s: Passed %s tests; %s discarded\r", name, s, d)
      }
      Console.flush
    }

  override def onTestResult(name: String, res: Test.Result) = {
    if(name == "") {
      print(List.fill(78)(' ').mkString)
      val s = (if(res.passed) "+ " else "! ") + pretty(res, prettyPrms)
      printf("\r%s\n", format(s, "", "", 75))
    } else {
      print(List.fill(78)(' ').mkString)
      val s = (if(res.passed) "+ " else "! ") + name + ": " +
        pretty(res, prettyPrms)
      printf("\r%s\n", format(s, "", "", 75))
    }
  }

}

object ConsoleReporter {

  /** Factory method, creates a ConsoleReporter with the
   *  the given verbosity */
  def apply(verbosity: Int = 0) = new ConsoleReporter(verbosity)

  @deprecated("(v1.8)")
  def propReport(s: Int, d: Int) = {
    if(d == 0) printf("\rPassed %s tests\r", s)
    else printf("\rPassed %s tests; %s discarded\r", s, d)
    Console.flush
  }

  @deprecated("(v1.8)")
  def propReport(pName: String, s: Int, d: Int) = {
    if(d == 0) printf("\r  %s: Passed %s tests\r", pName, s)
    else printf("\r  %s: Passed %s tests; %s discarded\r", pName, s, d)
    Console.flush
  }

  @deprecated("(v1.8)")
  def testReport(res: Test.Result) = {
    print(List.fill(78)(' ').mkString)
    val s = (if(res.passed) "+ " else "! ") + pretty(res, Params(0))
    printf("\r%s\n", format(s, "", "", 75))
    res
  }

  @deprecated("(v1.8)")
  def testStatsEx(res: Test.Result): Unit = testStatsEx("", res)

  def testStatsEx(msg: String, res: Test.Result) = {
    lazy val m = if(msg.length == 0) "" else msg + ": "
    res.status match {
      case Test.Proved(_) => {}
      case Test.Passed => {}
      case f @ Test.Failed(_, _) => error(m + f)
      case Test.Exhausted => {}
      case f @ Test.GenException(_) => error(m + f)
      case f @ Test.PropException(_, _, _) => error(m + f)
    }
  }

}

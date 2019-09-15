/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest

import scala.annotation.elidable, elidable.ASSERTION
import scala.tools.nsc.util.stackTraceHeadString
import scala.util.control.NonFatal

/** A class for testing code that asserts.
 */
abstract class RunTest extends Runnable {
  protected def expectFailure: Boolean = false

  @elidable(ASSERTION)
  protected def assertsOn: Boolean = true   // elides to false

  private def fail(): Unit = new AssertionError("Assert facility is disabled!").printStackTrace()

  // verifyAssertsEnabled(assertsOn)
  protected def verifyAssertsEnabled(flag: Boolean): Unit = if (!flag) fail()

  // run(assertsOn)(test)
  protected def run(flag: Boolean)(body: => Unit): Unit = {
    verifyAssertsEnabled(flag)
    body
  }

  def main(args: Array[String]) =
    try {
      run()
      if (expectFailure) new AssertionError("Expected exceptional condition!").printStackTrace()
    } catch {
      case e: AssertionError if expectFailure => println(stackTraceHeadString(e))
      case NonFatal(t: Throwable)             => t.printStackTrace()
    }
}

/** A test that throws an AssertionError. One frame is printed to verify what asserted.
 */
abstract class BadRunTest extends RunTest {
  override protected final def expectFailure: Boolean = true
}

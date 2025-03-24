/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest

import java.lang.System.{out => sysout}

/** A trait for testing repl's javap command
 *  or possibly examining its output.
 */
abstract class JavapTest extends ReplTest {

  /** Your Assertion Here, whatever you want to affirm.
   *  Assertions must be satisfied by all flavors of javap
   *  and should not be fragile with respect to compiler output.
   */
  def yah(res: Seq[String]): Boolean

  def baddies = List(":javap unavailable", ":javap not yet working")

  // give it a pass if javap is broken
  override def show() = try {
    val res = eval().toSeq
    val unsupported = res exists (s => baddies exists (s contains _))
    assert ((unsupported || yah(res)), res.mkString("","\n","\n"))
  } catch { case ae: AssertionError => ae.printStackTrace(sysout) }
}

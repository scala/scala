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

import org.junit.runner.JUnitCore
import org.junit.runner.notification.{Failure, RunListener}

import scala.tools.nsc.util.Exceptional

abstract class JUnitTest(classes: Class[_]*) extends App {

  val junit = new JUnitCore
  junit.addListener(new RunListener {
    override def testFailure(failure: Failure): Unit = {
      println(failure)
      val ex = failure.getException
      if(ex != null) {
        val seen = new java.util.IdentityHashMap[Throwable, Object]
        @scala.annotation.tailrec
        def trimStack(ex: Throwable): Unit = if (ex != null && !seen.containsKey(ex)) {
          seen.put(ex, null)
          ex.setStackTrace(ex.getStackTrace.takeWhile(!_.getClassName.startsWith("org.junit.runners.")))
          trimStack(ex.getCause)
        }

        val unwrapped = Exceptional.rootCause(ex)
        trimStack(unwrapped)
        unwrapped.printStackTrace()
      }
    }
  })
  junit.run(classes: _*)
}

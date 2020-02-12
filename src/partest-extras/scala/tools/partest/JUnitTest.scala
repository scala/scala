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

import java.io.{PrintWriter, StringWriter}

import org.junit.internal.TextListener
import org.junit.runner.JUnitCore
import org.junit.runner.notification.{Failure, RunListener}

import scala.collection.JavaConverters._

abstract class JUnitTest(classes: Class[_]*) extends App {

  val junit = new JUnitCore
  junit.addListener(new RunListener {
    override def testFailure(failure: Failure): Unit = {
      println(failure)
      val ex = failure.getException
      if(ex != null) {
        val sw = new StringWriter()
        val out = new PrintWriter(sw)
        ex.printStackTrace(out)
        out.flush()
        val lines = sw.getBuffer.toString.split('\n')
        lines.iterator.takeWhile(s => !s.contains("at org.junit.runners.")).foreach(println)
      }
    }
  })
  junit.run(classes: _*)
}

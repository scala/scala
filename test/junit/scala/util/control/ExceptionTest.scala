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

package scala.util

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.collection.mutable.ListBuffer

import scala.util.control.Exception._

@RunWith(classOf[JUnit4])
class ExceptionTest {

  @Test
  def andFinally(): Unit = {

    locally {
      val audit = ListBuffer[Int]()
      val katch = nonFatalCatch[Unit].andFinally(audit append 1)
      val result = katch[AnyVal](10)
      assertEquals(10, result)
      assertEquals(1 :: Nil, audit.toList)
    }

    locally {
      val audit = ListBuffer[Int]()
      val katch = nonFatalCatch[Unit].andFinally(audit append 1).andFinally(audit append 2)
      val result = katch[AnyVal](20)
      assertEquals(20, result)
      assertEquals(1 :: 2 :: Nil, audit.toList)
    }
  }
}

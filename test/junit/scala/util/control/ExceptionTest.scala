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
      val result = katch(10)
      assertEquals(result, 10)
      assertEquals(audit.toList, 1 :: Nil)
    }

    locally {
      val audit = ListBuffer[Int]()
      val katch = nonFatalCatch[Unit].andFinally(audit append 1).andFinally(audit append 2)
      val result = katch(20)
      assertEquals(result, 20)
      assertEquals(audit.toList, 1 :: 2 :: Nil)
    }
  }
}
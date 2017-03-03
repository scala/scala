/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2016-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
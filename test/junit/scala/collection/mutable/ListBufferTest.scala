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

package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ListBufferTest {
  @Test def `toSeq and friends should not expose mutability (scala/bug#11869)` {
    def check[CC[x] <: collection.Traversable[x]](export: ListBuffer[Int] => CC[Int]) {
      val buf = ListBuffer.empty[Int]
      buf += 1; buf += 2
      val res = export(buf)
      buf += 3
      assertEquals(3, buf.size)
      assertEquals(2, res.size)
    }
    check(_.toList)
    check(_.toSeq)
    check(_.toIterable)
    check(_.toStream)
  }
}

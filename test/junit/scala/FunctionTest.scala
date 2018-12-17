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

package scala

import org.junit.{Assert, Test}

class FunctionTest {

  @Test
  def `testFunction#identity`(): Unit = {
    val input: Int = 1
    val result = Function.identity[Int].apply(input)
    Assert.assertEquals(input, result)
    Assert.assertTrue(result.getClass == classOf[Int])
  }
}

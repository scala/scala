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

/** A class for testing parser output.
 *  Just supply the `code` and update the check file.
 */
abstract class ParserTest extends DirectTest {

  override def extraSettings: String = "-usejavacp -Ystop-after:parser -Xprint:parser"

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    compile()
    System.setErr(prevErr)
  }
}

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

package scala.async
package run
package uncheckedBounds

import org.junit.{Test, Assert}
import scala.async.TreeInterrogation

class UncheckedBoundsSpec {
  @Test def insufficientLub_SI_7694(): Unit = {
    eval( s"""
      object Test {
        import _root_.scala.async.run.toughtype._
        import _root_.scala.async.internal.AsyncId.{async, await}
        async {
          (if (true) await(null: L[A, A]) else await(null: L[B, B]))
        }
    }
    """, compileOptions = s"-cp ${toolboxClasspath} ")
  }

  @Test def insufficientLub_SI_7694_ScalaConcurrent(): Unit = {
    eval( s"""
      object Test {
        import _root_.scala.async.run.toughtype._
        import _root_.scala.async.Async.{async, await}
        import scala.concurrent._
        import scala.concurrent.ExecutionContext.Implicits.global
        async {
          (if (true) await(null: Future[L[A, A]]) else await(null: Future[L[B, B]]))
        }
    }
    """, compileOptions = s"-cp ${toolboxClasspath} ")
  }

}

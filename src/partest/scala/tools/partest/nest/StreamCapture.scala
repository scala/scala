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

package scala.tools.partest.nest

import java.io.{Console => _, _}
import java.nio.charset.Charset

import scala.util.Using

object StreamCapture {
  def savingSystem[A](body: => A): A = {
    val savedOut  = System.out
    val savedErr  = System.err
    try body
    finally {
      System.setErr(savedErr)
      System.setOut(savedOut)
    }
  }

  /** Set err to out. */
  def redirErr[A](body: => A): A = savingSystem {
    System.setOut(System.err)
    body
  }

  def capturingOutErr[A](output: OutputStream)(body: => A): A = {
    val charset = Charset.defaultCharset()
    Using.resource(new PrintStream(output, /*autoflush=*/true, charset.name())) { printStream =>
      savingSystem {
        System.setOut(printStream)
        System.setErr(printStream)
        Console.withErr(printStream) {
          Console.withOut(printStream) {
            body
          }
        }
      }
    }
  }

  def withExtraProperties[A](extra: Map[String, String])(action: => A): A = {
    val saved = System.getProperties()
    val modified = new java.util.Properties()
    // on Java 9, we need to cast our way around this:
    // src/main/scala/scala/tools/partest/nest/StreamCapture.scala:44: ambiguous reference to overloaded definition,
    // both method putAll in class Properties of type (x$1: java.util.Map[_, _])Unit
    // and  method putAll in class Hashtable of type (x$1: java.util.Map[_ <: Object, _ <: Object])Unit
    // match argument types (java.util.Properties)
    (modified: java.util.Hashtable[AnyRef, AnyRef]).putAll(saved)
    extra.foreach { case (k, v) => modified.setProperty(k, v) }
    // Trying to avoid other threads seeing the new properties object prior to the new entries
    // https://github.com/scala/scala/pull/6391#issuecomment-371346171
    // (JDK 22 deprecates `storeFence`; once we drop JDK 8 we can use the VarHandles one instead)
    UnsafeAccess.U.storeFence(): @annotation.nowarn("cat=deprecation")
    System.setProperties(modified)
    try {
      action
    } finally {
      System.setProperties(saved)
    }
  }
}

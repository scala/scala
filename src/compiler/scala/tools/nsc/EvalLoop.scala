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

package scala.tools.nsc

import scala.annotation.tailrec
import scala.io.StdIn
import java.io.EOFException

trait EvalLoop {
  def prompt: String

  def loop(action: (String) => Unit): Unit = {
    @tailrec def inner(): Unit = {
      Console.print(prompt)
      val line = try StdIn.readLine() catch { case _: EOFException => null }
      if (line != null && line != "") {
        action(line)
        inner()
      }
    }
    inner()
  }
}

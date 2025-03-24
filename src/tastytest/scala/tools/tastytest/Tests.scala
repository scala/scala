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

package scala.tools.tastytest

import scala.util.{ Try, Success, Failure }

object Tests {

  def printSummary(suite: String, result: Try[Unit]) = result match {
    case Success(_)   => printsuccessln(s"$suite suite passed!")
    case Failure(err) => printerrln(s"ERROR: $suite suite failed: ${err.getClass.getName}: ${err.getMessage}")
  }

  def suite(name: String, willRun: Boolean)(runner: => Try[Unit]): Option[Boolean] = {
    if (willRun) {
      println(s"Performing suite $name")
      val result = runner
      printSummary(name, result)
      Some(result.isSuccess)
    }
    else {
      None
    }
  }

}

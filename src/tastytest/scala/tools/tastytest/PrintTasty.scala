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

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object PrintTasty extends Script.Command {

  def printTasty(tasty: String)(implicit cl: Dotc.ClassLoader): Try[Unit] =
    Dotc.mainMethod("dotty.tools.dotc.core.tasty.TastyPrinter")(Seq(tasty))

  val commandName: String = "printTasty"
  val describe: String = s"$commandName <tasty: File>"

  def process(args: String*): Int = {
    if (args.length != 1) {
      println(red(s"please provide 1 argument in sub-command: $describe"))
      return 1
    }
    Dotc.processIn { implicit scala3classloader =>
      printTasty(tasty = args.head) match {
        case Success(_) => 0
        case Failure(err) =>
          println(red(s"failed to print tasty: $err"))
          1
      }
    }
  }

}

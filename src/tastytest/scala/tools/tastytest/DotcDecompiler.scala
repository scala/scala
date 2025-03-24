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

object DotcDecompiler extends Script.Command {

  private def dotcProcess(args: Seq[String])(implicit cl: Dotc.ClassLoader) =
    Dotc.processMethod("dotty.tools.dotc.decompiler.Main")(args)

  def decompile(source: String, additionalSettings: Seq[String])(implicit cl: Dotc.ClassLoader): Try[Boolean] =
    dotcProcess(("-usejavacp" +: additionalSettings :+ source))

  val commandName: String = "dotcd"
  val describe: String = s"$commandName <tasty: File> <args: String*>"

  def process(args: String*): Int = {
    if (args.length < 1) {
      println(red(s"please provide at least 1 argument in sub-command: $describe"))
      return 1
    }
    val Seq(tasty, additionalSettings @ _*) = args: @unchecked
    Dotc.processIn { implicit scala3classloader =>
      val success = decompile(tasty, additionalSettings).get
      if (success) 0 else 1
    }
  }

}

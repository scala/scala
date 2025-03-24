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

trait Script extends Script.Command {

  def subcommands: Seq[Script.Command]

  final def process(args: String*): Int = {
    if (args.isEmpty) {
      println(red("Please provide at least one sub-command"))
      return 1
    }
    val Seq(command, args0 @ _*) = args: @unchecked
    subcommands.collectFirst {
      case subcommand if subcommand.commandName == command => subcommand.process(args0:_*)
    }.getOrElse {
      println(red(s"unrecognised sub-command $command, try from the following $describe"))
      1
    }
  }

  final def main(args: Array[String]): Unit = sys.exit(process(args.toIndexedSeq: _*))

  final def describe = subcommands.map(sub => s"$commandName ${sub.describe}").mkString("options:\n  ","\n  ","")
}

object Script {

  trait Command {
    def describe: String
    def commandName: String
    def process(args: String*): Int
  }

}

/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd
package program

import nsc.io._

object Scmp {
  private val scmpUsage = """
    |Usage: scmp [options] <cmd line>
    |Example: scmp --p1 '-no-specialization -Ydebug' scalac src/library/scala/Function1.scala
    |
    |Note: the command line must start with a path to scalac.
    |""".stripMargin
  private val scmpOptions = List(
    "p1" -> "options for the first run only",
    "p2" -> "options for the second run only"
  )
  private val scmpInfo = Simple.scalaProgramInfo("scmp", scmpUsage)
  lazy val ScmpSpec = Simple(scmpInfo, Nil, scmpOptions, x => returning(x)(_.onlyKnownOptions = false))

  def main(args0: Array[String]): Unit = {
    if (args0.isEmpty)
      return println(scmpUsage)

    val runner = ScmpSpec instance args0
    import runner._

    val p1args = parsed.getOrElse("--p1", "")
    val p2args = parsed.getOrElse("--p2", "")

    if (p1args.isEmpty && p2args.isEmpty)
      return println("At least one of --p1 and --p2 must be given.")
    if (residualArgs.isEmpty)
      return println("There is no command to run.")

    def createCmd(extras: String) =
      fromArgs(residualArgs.patch(1, toArgs(extras), 0))

    def runCmd(cmd: String) = {
      val output = Process(cmd, redirect = true).slurp()

      returning(File.makeTemp())(_ writeAll output)
    }

    val cmds = List(p1args, p2args) map createCmd
    println(cmds.mkString("Running command lines:\n  ", "\n  ", ""))

    val files = cmds map runCmd map (_.path)
    val diff = Process("diff %s %s".format(files: _*)).slurp()

    if (diff.isEmpty) println("No differences.")
    else println(diff)
  }
}

/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd
package program

import scala.reflect.internal.JvmClassInfo
import scala.tools.nsc.io.Directory

object DumpClass {
  private val usage = """
    |Usage: dump-class [options] <path> <path> ...
    |
    |Parses and dumps the bytecode of all classes found at the given paths.
    |""".stripMargin

  private val unaryOps = List(
    "signatures" -> "dump signatures"
  )
  private val info = Simple.scalaProgramInfo("dump-class", usage)
  private val spec = Simple(info, unaryOps, Nil, null)

  def deepInfos(dir: String) = {
    val files = Directory(dir).deepFiles.toList filter (_ hasExtension "class")
    files.sortBy(_.path) map (f => (f.path, JvmClassInfo fromPath f.path))
  }

  def main(args: Array[String]): Unit = {
    val runner = spec instance args
    import runner._

    if (args.isEmpty)
      println(usage)
    else
      (residualArgs flatMap deepInfos) sortBy (_._1) map (_._2) foreach println
  }
}

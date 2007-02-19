/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc


import compat.StringBuilder

/** A class representing command line info for scalac */
class CompilerCommand(arguments: List[String], val settings: Settings,
                      error: String => unit, interactive: boolean) {

  private var fs: List[String] = List()

  /** All files to compile */
  def files: List[String] = fs.reverse

  /** The name of the command */
  val cmdName = "scalac"

  /** The file extension of files that the compiler can process */
  val fileEnding = ".scala"

  /** A message explaining usage and options */
  def usageMsg: String = {
    val helpSyntaxColumnWidth: int =
      Iterable.max(settings.allSettings map (. helpSyntax.length()))
    def format(s: String): String = {
      val buf = new StringBuilder(s)
      var i = s.length()
      while (i < helpSyntaxColumnWidth) { buf.append(' '); i = i + 1 }
      buf.toString()
    }
    settings.allSettings
      .map(setting =>
           format(setting.helpSyntax) + "  " + setting.helpDescription)
      .mkString(
                "Usage: " + cmdName + " <options | source files>\n" +
                "where possible options include: \n  ",
                "\n  ",
                "\n")
  }

  // initialization
  var args = arguments
  var ok = true

  while (!args.isEmpty && ok) {
    if (args.head startsWith "-") {
      if (interactive) {
        error("no options can be given in interactive mode")
        ok = false
      } else {
        val args0 = args
        for (val setting <- settings.allSettings)
          if (args eq args0)
            args = setting.tryToSet(args)

        if (args eq args0) {
          error("bad option: '" + args.head + "'")
          ok = false
        }
      }
    } else if (settings.Xscript.value || args.head.endsWith(fileEnding)) {
      fs = args.head :: fs
      args = args.tail
    } else if (args.head.length == 0) {//quick fix
      args = args.tail
    } else {
      error("don't know what to do with " + args.head)
      ok = false
    }
  }
}

/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc


/** A class representing command line info for scalac */
class CompilerCommand(arguments: List[String], val settings: Settings,
                      error: String => Unit, interactive: Boolean) {

  private var fs: List[String] = List()

  /** All files to compile */
  def files: List[String] = fs.reverse

  /** The name of the command */
  val cmdName = "scalac"

  /** The file extension of files that the compiler can process */
  def fileEnding = Properties.fileEndingString //todo: lazy val

  private val helpSyntaxColumnWidth: Int =
    Iterable.max(settings.allSettings map (_.helpSyntax.length))

  private def format(s: String): String = {
    val buf = new StringBuilder(s)
    var i = s.length
    while (i < helpSyntaxColumnWidth) { buf.append(' '); i += 1 }
    buf.toString()
  }

  /** A message explaining usage and options */
  def usageMsg: String = {
    settings.allSettings
      .filter(setting =>
              setting.isStandard &&
              (settings.doc.value == setting.isDocOption))
      .map(setting =>
           format(setting.helpSyntax) + "  " + setting.helpDescription)
      .mkString("Usage: " + cmdName + " <options> <source files>\n" +
                "where possible standard options include:\n  ",
                "\n  ",
                "\n")
  }

  /** A message explaining usage and options */
  def xusageMsg: String = {
    settings.allSettings
      .filter(setting =>
              setting.isAdvanced &&
              (settings.doc.value == setting.isDocOption))
      .map(setting =>
           format(setting.helpSyntax) + "  " + setting.helpDescription)
      .mkString("Possible advanced options include:\n  ",
                "\n  ",
                "\n")
  }

  /** A message explaining usage and options */
  def yusageMsg: String = {
    settings.allSettings
      .filter(setting =>
              setting.isPrivate &&
              (settings.doc.value == setting.isDocOption))
      .map(setting =>
           format(setting.helpSyntax) + "  " + setting.helpDescription)
      .mkString("Possible private options include:\n  ",
                "\n  ",
                "\n")
  }


  /** Whether the command was processed okay */
  var ok = true

  /** Process the arguments and update the settings accordingly.
      This method is called only once, during initialization.  */
  protected def processArguments() {
    // initialization
    var args = arguments

    while (!args.isEmpty && ok) {
      if (args.head startsWith "-") {
	if (interactive) {
          error("no options can be given in interactive mode")
          ok = false
        } else {
          val args0 = args
          for (setting <- settings.allSettings)
            if (args eq args0)
              args = setting.tryToSet(args)

          if (args eq args0) {
            error("bad option: '" + args.head + "'")
            ok = false
          }
        }
      } else if ((settings.script.value != "") || args.head.endsWith(fileEnding)) {
        fs = args.head :: fs
        args = args.tail
      } else if (args.head.length == 0) {//quick fix
        args = args.tail
      } else {
        error("don't know what to do with " + args.head)
        ok = false
      }
    }
    ok &&= settings.checkDependencies
  }

  processArguments()
}

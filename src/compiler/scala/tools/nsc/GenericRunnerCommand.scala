/* NSC -- new Scala compiler
 * Copyright 2007 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

/** A command for ScriptRunner */
class GenericRunnerCommand(
  allargs: List[String],
  override val settings: GenericRunnerSettings,
  error: String => Unit)
extends CompilerCommand(allargs, settings, error, false, false)
{
  def this(allargs: List[String], error: String=>Unit) =
    this(allargs, new GenericRunnerSettings(error), error)

  def this(allargs: List[String]) =
    this(allargs, str => Console.println("Error: " + str))

  /** name of the associated compiler command */
  override val cmdName = "scala"
  val compCmdName = "scalac"

  /** What to run.  If it is None, then the interpreter should be started */
  var thingToRun: Option[String] = None

  /** Arguments to pass to the object or script to run */
  var arguments: List[String] = Nil

  override protected def processArguments() {
    var args = allargs

    while (!args.isEmpty && ok && args.head.startsWith("-")) {
      val args0 = args
      args = settings parseParams args
      if (args eq args0) {
        error("bad option: '" + args.head + "'")
        ok = false
      }
    }

    if (!args.isEmpty) {
      thingToRun = Some(args.head)
      arguments = args.tail
    }
  }

  // we can safely call processArguments since we passed the superclass shouldProcessArguments=false
  processArguments()

  override def usageMsg = {
    cmdName + " [ <option> ]... [<torun> <arguments>]\n" +
    "\n" +
    "All options to "+compCmdName+" are allowed.  See "+compCmdName+" -help.\n" +
    "\n" +
    "<torun>, if present, is an object or script file to run.\n" +
    "If no <torun> is present, run an interactive shell.\n" +
    "\n" +
    "Option -howtorun allows explicitly specifying how to run <torun>:\n" +
    "    script: it is a script file\n" +
    "    object: it is an object name\n" +
    "    guess: (the default) try to guess\n" +
    "\n" +
    "Option -i requests that a file be pre-loaded.  It is only\n" +
    "meaningful for interactive shells.\n" +
    "\n" +
    "Option -e requests that its argument be executed as Scala code.\n" +
    "\n" +
    "Option -savecompiled requests that the compiled script be saved\n" +
    "for future use.\n" +
    "\n" +
    "Option -nocompdaemon requests that the fsc offline compiler not be used.\n" +
    "\n" +
    "Option -Dproperty=value sets a Java system property.\n"
  }
}

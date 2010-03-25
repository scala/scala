/* NSC -- new Scala compiler
 * Copyright 2007 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

/** A command for ScriptRunner */
class GenericRunnerCommand(
  args: List[String],
  override val settings: GenericRunnerSettings)
extends CompilerCommand(args, settings) {

  def this(args: List[String], error: String => Unit) =
    this(args, new GenericRunnerSettings(error))

  def this(args: List[String]) =
    this(args, str => Console.println("Error: " + str))

  /** name of the associated compiler command */
  override val cmdName = "scala"
  val compCmdName = "scalac"

  // change CompilerCommand behavior
  override def shouldProcessArguments: Boolean = false

  /** thingToRun: What to run.  If it is None, then the interpreter should be started
   *  arguments: Arguments to pass to the object or script to run
   */
  val (thingToRun, arguments) = settings.processArguments(args, false)._2 match {
    case Nil      => (None, Nil)
    case hd :: tl => (Some(hd), tl)
  }

  override def usageMsg = """
%s [ <option> ]... [<torun> <arguments>]

All options to %s are allowed.  See %s -help.

<torun>, if present, is an object or script file to run.
If no <torun> is present, run an interactive shell.

Option -howtorun allows explicitly specifying how to run <torun>:
    script: it is a script file
    object: it is an object name
    guess: (the default) try to guess

Option -i requests that a file be pre-loaded.  It is only
meaningful for interactive shells.

Option -e requests that its argument be executed as Scala code.

Option -savecompiled requests that the compiled script be saved
for future use.

Option -nocompdaemon requests that the fsc offline compiler not be used.

Option -Dproperty=value sets a Java system property.
""".format(cmdName, compCmdName, compCmdName)
}

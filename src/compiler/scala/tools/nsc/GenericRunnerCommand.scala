/* NSC -- new Scala compiler
 * Copyright 2007 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

/** A command for ScriptRunner */
class GenericRunnerCommand(
  val allargs: List[String],
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

  /** thingToRun: What to run.  If it is None, then the interpreter should be started
   *  arguments: Arguments to pass to the object or script to run
   *
   *  we can safely process arguments since we passed the superclass shouldProcessArguments=false
   */
  val (thingToRun, arguments) = (settings.processArguments(allargs, false))._2 match {
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

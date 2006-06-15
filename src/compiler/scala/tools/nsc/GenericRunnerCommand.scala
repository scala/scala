/* NSC -- new Scala compiler
 * Copyright 2006 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

/** A command for ScriptRunner */
class GenericRunnerCommand(allargs: List[String], error: String => Unit) {
  def this(allargs: List[String]) = this(allargs, str:String => {
    Console.println("Error: " + str)
  })

  /** Settings specified by this command */
  val settings = new GenericRunnerSettings(error)

  /** Whether the command was parsed correctly */
  var ok = true

  /** What to run.  If it is None, then the interpreter should be started */
  var thingToRun: Option[String] = None

  /** Arguments to pass to the object or script to run */
  var arguments: List[String] = Nil

  private def parseArguments: Unit = {
    var args = allargs

    while (!args.isEmpty && ok && args.head.startsWith("-")) {
      val args0 = args
      for (val setting <- settings.allSettings)
        args = setting.tryToSet(args)
      if (args eq args0) {
        error("unknown option: '" + args.head + "'")
        ok = false
      }
    }

    if(!args.isEmpty) {
      thingToRun = Some(args.head)
      arguments = args.tail
    }
  }
  parseArguments

  val usageMessage = {
    "scala [ <compiler-option> | -howtorun:how ]... [<torun> <arguments>]\n" +
    "\n" +
    "<compiler-option>'s are as for scalac; see scalac -help.\n" +
    "<torun>, if present, is an object or script file to run.\n" +
    "If no <torun> is present, run an interactive interpreter.\n" +
    "-howtorun allows explicitly specifying how to run <torun>:\n" +
    "    script: it is a script file\n" +
    "    object: it is an object name\n" +
    "    guess: (the default) try to guess\n"
  }
}

/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc;

import scala.tools.util.{Position, Reporter, ConsoleReporter}

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main {

  val PRODUCT: String =
    System.getProperty("scala.product", "scalac");
  val VERSION: String =
    System.getProperty("scala.version", "unknown version");
  val versionMsg = PRODUCT + " " + VERSION + " -- (c) 2002-04 LAMP/EPFL";

  private var reporter: ConsoleReporter = _;

  def error(msg: String): unit =
    reporter.error(new Position(PRODUCT),
		   msg + "\n  " + PRODUCT + " -help  gives more information");

  def process(args: Array[String]): unit = {
    reporter = new ConsoleReporter();
    val command = new CompilerCommand(args, error);
    reporter.prompt(command.settings.prompt.value);
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (command.settings.help.value || command.files.isEmpty)
      reporter.info(null, command.usageMsg, true)
    else {
      try {
	val compiler = new Global(command.settings, reporter);
	compiler.compile(command.files);
      } catch {
	case ex @ FatalError(msg) =>
	  if (command.settings.debug.value)
	    ex.printStackTrace();
	  System.out.println("fatal error: " + msg);
      }
      reporter.printSummary()
    }
  }

  def main(args: Array[String]): unit = {
    process(args);
    System.exit(if (reporter.errors() > 0) 1 else 0);
  }
}

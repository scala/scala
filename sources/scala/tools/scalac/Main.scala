/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

import scala.tools.util.{AbstractReporter, ConsoleReporter};
import scalac.{CompilerCommand, Global => scalac_Global};
import scalac.symtab.classfile.CLRTypes;

package scala.tools.scalac {

/** The main class for SoCoS, a compiler for the programming
 *  language Scala.
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
object Main {

  val PRODUCT: String =
    System.getProperty("scala.product", "socos");
  val VERSION: String =
    System.getProperty("scala.version", "unknown version");

  def main(args: Array[String]): unit = main1( true, args );

  // ant task needs to be aware of reporter.errors
  var reporter: AbstractReporter = _;

  def main1( exitOnError:boolean, args: Array[String] ):unit = {
    val reporter = new ConsoleReporter();
    this.reporter = reporter;
    val command = new CompilerCommand(
      PRODUCT, VERSION, reporter, new CompilerPhases());
    var ok = true;
    if (command.parse(args) && command.files.list.size() > 0) {
      if (command.target.value == scalac_Global.TARGET_MSIL) {
	try { CLRTypes.init(command); }
	catch { case e: Error =>
          e.printStackTrace();
          ok = false;
        }
      }
      if (ok) {
        val timer = scalac_Global.getTimer(reporter);
        timer.start();
	val global = new Global(command, timer, false);
        try {
	  val units = global.compile(command.files.toArray(), false);
	  if (reporter.errors() == 0)
            if (!global.PHASE.CODEGEN.hasSkipFlag()) global.dump(units);
        } catch {
          case e: scala.tools.util.debug.AbortError =>
            if (global.debug)
              e.printStackTrace();
            else
              global.error("Internal compiler error: " + e.getMessage()
                           + "; use -debug to see a stack trace");
        }
        timer.stop("total");
	reporter.printSummary();
      }
    }
    if( exitOnError ) {
      System.exit(if (reporter.errors() > 0 || !ok) 1 else 0);
    }
  }
}
}

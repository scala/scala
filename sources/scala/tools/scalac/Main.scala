/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

import scalac.util.Reporter;
import scalac.{CompilerCommand, Global => scalac_Global};
import scalac.symtab.classfile.CLRPackageParser;

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

  def main1( exitOnError:boolean, args: Array[String] ):unit = {
    val reporter = new Reporter();
    val command = new CompilerCommand(
      PRODUCT, VERSION, reporter, new CompilerPhases());
    var ok = true;
    if (command.parse(args) && command.files.list.size() > 0) {
      if (command.target.value == scalac_Global.TARGET_MSIL) {
	try { CLRPackageParser.init(command); }
	catch { case e: Error => ok = false; }
      }
      if (ok) {
	val global = new Global(command);
	global.compile(command.files.toArray(), false);
	global.stop("total");
	global.reporter.printSummary();
      }
    }
    if( exitOnError ) {
      System.exit(if (reporter.errors() > 0 || !ok) 1 else 0);
    }
  }
}
}

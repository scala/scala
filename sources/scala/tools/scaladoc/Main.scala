/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{Global => scalac_Global};
import scala.tools.scalac.{Global, CompilerPhases};
import scala.tools.util.Reporter;

package scala.tools.scaladoc {

/**
 * The main class for scaladoc, an HTML documentation generator
 * for the programming language Scala.
 *
 * @author     Vincent Cremet, Stephane Micheloud
 * @version    1.0
 */
object Main {

  val PRODUCT: String =
    System.getProperty("scala.product", "scaladoc");
  val VERSION: String =
    System.getProperty("scala.version", "1.0");

  def main(args: Array[String]): Unit = {
    val reporter = new Reporter();
    val phases = new CompilerPhases(); {
      // we skip all phases between ANALYZER and TERMINAL.
      val array = phases.phases();
      var i = 0;
      var skip = false;
      while (i < array.length - 1) {
        if (skip)
          array(i).addSkipFlag();
        else
          skip = array(i) == phases.ANALYZER;
        i = i + 1;
      }
    }
    val command = new HTMLGeneratorCommand(PRODUCT, VERSION, reporter, phases);
    if (command.parse(args) && command.files.list.size() > 0) {
      val global = new Global(command);
      global.compile(command.files.toArray(), false);
      if (reporter.errors() == 0) {
        val generator = new HTMLGenerator(global) {
          def newTypeIso(global: scalac_Global): TypeIsomorphism =
            new ScalaML(global);
        }
        generator.apply();
      }
      global.reporter.printSummary();
    }
    // System.exit(if (reporter.errors() > 0) 1 else 0);
  }

}
}

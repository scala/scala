/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import scalac.Global;
import scalac.util.Reporter;

/**
 * The main class for scaladoc, an HTML documentation generator
 * for the programming language Scala.
 *
 * @author     Vincent Cremet, Stephane Micheloud
 * @version    1.0
 */
public class Main {

    //########################################################################
    // Public Constants

    public static final String PRODUCT =
        System.getProperty("scala.product", "scaladoc");
    public static final String VERSION =
        System.getProperty("scala.version", "1.0");

    //########################################################################
    // Public Methods

    public static void main(String[] args) {
        Reporter reporter = new Reporter();
        HTMLGeneratorCommand command = new HTMLGeneratorCommand(
            PRODUCT, VERSION, reporter, new HTMLGeneratorPhases());
        if (command.parse(args) && command.files.list.size() > 0) {
            Global global = new Global(command);
            global.compile(command.files.toArray(), false);
            global.stop("total");
            global.reporter.printSummary();
        }
        // System.exit((reporter.errors() > 0) ? 1 : 0);
    }

    //########################################################################
}

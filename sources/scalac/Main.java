/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

import scalac.util.Reporter;

/** The main class for SoCoS, a compiler for the programming
 *  language Scala.
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public class Main {

    public static final String PRODUCT =
        System.getProperty("scala.product", "socos");
    public static final String VERSION =
        System.getProperty("scala.version", "unknown version");

    public static void main(String[] args) {
        Reporter reporter = new Reporter();
        CompilerCommand command = new CompilerCommand(
            PRODUCT, VERSION, reporter, new PhaseRepository());
        if (command.parse(args) && command.files.list.size() > 0) {
            Global global = new Global(command);
            global.compile(command.files.toArray(), false);
            global.stop("total");
            global.reporter.printSummary();
        }
        System.exit((reporter.errors() > 0) ? 1 : 0);
    }
}

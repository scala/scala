/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Main.java,v 1.2 2002/08/30 15:44:55 paltherr Exp $
// $Id$

package scalai;

import scalac.PhaseRepository;
import scalac.util.Reporter;

public class Main {

    //########################################################################
    // Public Constants

    public static final String PRODUCT =
        System.getProperty("scala.product", "scala");
    public static final String VERSION =
        System.getProperty("scala.version", "1.0");

    //########################################################################
    // Public Methods

    public static void main(String[] args) {
        Reporter reporter = new Reporter();
        InterpreterCommand command = new InterpreterCommand(
            PRODUCT, VERSION, reporter, new PhaseRepository());
        if (command.parse(args)) {
            Interpreter interpreter = new Interpreter(command);
            interpreter.main(command.files.toArray(),
                command.program.main, command.program.args);
        }
        System.exit((reporter.errors() > 0) ? 1 : 0);
    }

    //########################################################################
}

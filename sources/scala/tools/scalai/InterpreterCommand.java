/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: InterpreterCommand.java,v 1.3 2002/09/02 12:58:22 paltherr Exp $
// $Id$

package scala.tools.scalai;

import scalac.CompilerPhases;
import scalac.CompilerCommand;
import scala.tools.util.Reporter;
import scalac.util.StringOptionParser;
import scalac.util.BooleanOptionParser;
import scalac.util.ScalaProgramArgumentParser;

public class InterpreterCommand extends CompilerCommand {

    //########################################################################
    // Public Fields

    public final StringOptionParser script;
    public final BooleanOptionParser interactive;
    public final BooleanOptionParser nologo;
    public final BooleanOptionParser emacs;
    public final ScalaProgramArgumentParser program;

    //########################################################################
    // Public Constructors

    public InterpreterCommand(String product, String version,
        Reporter reporter, CompilerPhases phases)
    {
        this(product, version, "<source files> [-- <module> <args>]",
            reporter, phases);
    }

    public InterpreterCommand(String product, String version, String syntax,
        Reporter reporter, CompilerPhases phases)
    {
        super(product, version, syntax, reporter, phases);

        this.script = new StringOptionParser(this,
            "c", "Evaluate <string> and print result",
            "string", null);

        this.interactive = new BooleanOptionParser(this,
            "interactive", "Start interpreter in interactive mode",
            false);

        this.nologo = new BooleanOptionParser(this,
            "nologo", "Print no logo at interactive interpreter start",
            false);

        this.emacs = new BooleanOptionParser(this,
            "emacs", "Use Emacs editing mode",
            false);

        this.program = new ScalaProgramArgumentParser(this);

        remove(outpath);
        remove(target);

        add(0, script);
        add(1, interactive);
        add(2, nologo);
        add(3, emacs);
        add(parsers().indexOf(unknown_options), program);
    }

    //########################################################################
}

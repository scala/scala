/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import scalac.CompilerPhases;
import scalac.PhaseDescriptor;

/**
 * The class <code>HTMLGeneratorPhases</code> defines the set
 * of processing phases belonging the the HTML generator tool.
 */
public class HTMLGeneratorPhases extends CompilerPhases {

    //########################################################################
    // Public Fields

    /**
     * The phase descriptor of the HTML generator.
     */
    public final PhaseDescriptor HTMLGENERATOR;

    //########################################################################
    // Public Constructors

    /**
     * Creates an instance variable.
     */
    public HTMLGeneratorPhases() {
        this.HTMLGENERATOR = new PhaseDescriptor(
            "html generator",
            "generate html documentation",
            "generating html",
            scala.tools.scaladoc.HTMLGeneratorPhase.class);

        int pos = insertAfter(HTMLGENERATOR, ANALYZER);
        PhaseDescriptor[] array = phases();
        // we skip all phases between HTMLGENERATOR and TERMINAL.
        for (int i = pos + 1; i < array.length - 1; i++)
            array[i].addSkipFlag();
    }

    //########################################################################
}

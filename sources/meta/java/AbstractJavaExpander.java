/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.java;

import meta.util.TextWriter;
import meta.util.AbstractFileExpander;

/** A base class for java file expanders. */
public abstract class AbstractJavaExpander extends AbstractFileExpander {

    //########################################################################
    // Public Fields

    /** The underlying java writer */
    public final JavaWriter writer;

    //########################################################################
    // Public Constructors

    public AbstractJavaExpander() {
        this.writer = new JavaWriter(getPackage());
    }

    //########################################################################
    // Public Methods

    /** Returns the TextWriter in which this expander writes. */
    public TextWriter getTextWriter() {
        return writer.getTextWriter();
    }

    /** Returns the suffix of the target file. Returns "java". */
    public String getTargetSuffix() {
        return "java";
    }

    /** Prints the import statements. */
    public void printImports() {
        writer.printImports();
    }

    //########################################################################
}

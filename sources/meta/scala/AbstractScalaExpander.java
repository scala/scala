/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scala;

import meta.java.JavaWriter;
import meta.util.TextWriter;
import meta.util.AbstractFileExpander;

/** A base class for scala file expanders. */
public abstract class AbstractScalaExpander extends AbstractFileExpander {

    //########################################################################
    // Public Fields

    /** The underlying java writer */
    public final JavaWriter writer;

    //########################################################################
    // Public Constructors

    public AbstractScalaExpander() {
        this.writer = new JavaWriter(getPackage());
    }

    //########################################################################
    // Public Methods

    /** Returns the TextWriter in which this expander writes. */
    public TextWriter getTextWriter() {
        return writer.getTextWriter();
    }

    /** Returns the suffix of the target file. Returns "scala". */
    public String getTargetSuffix() {
        return "scala";
    }

    //########################################################################
}

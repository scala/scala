/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scala;

public class MetaTuple extends AbstractScalaExpander {

    //########################################################################
    // Public Fields

    public final int arity;

    //########################################################################
    // Public Constructors

    public MetaTuple(int arity) {
        this.arity = arity;
    }

    //########################################################################
    // Public Methods

    public String getTargetBaseName() {
        return super.getTargetBaseName() + arity;
    }

    public void printn() {
        writer.print(arity);
    }

    public void printTParams() {
        for (int i = 0; i < arity; i++) {
            if (i > 0) writer.print(", ");
            writer.print("T").print(i);
        }
    }

    public void printVParams() {
        for (int i = 0; i < arity; i++) {
            if (i > 0) writer.print(", ");
            writer.print("_").print(i).print(": T").print(i);
        }
    }

    //########################################################################
}

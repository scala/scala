/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.checkers;

import scalac.ast.*;
import scalac.Global;

public abstract class Checker extends Traverser {
    protected final Global global;

    public Checker(Global global) {
        this.global = global;
    }

    public boolean implies(boolean b1, boolean b2) {
        return (!b1) | b2;
    }

    public void verify(Tree tree, boolean b, String name, String message) {
        if (! b) {
            System.err.println("ERROR: Condition '" + name + "' violated (after "
                               + global.currentPhase + ")!");
            System.err.println(message);
            System.err.println(tree);
            System.err.println();
        }
    }

    abstract public void check(Tree tree);

    public void traverse(Tree tree) {
        check(tree);
        super.traverse(tree);
    }
}

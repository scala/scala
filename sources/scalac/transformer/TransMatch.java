/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import java.io.*;
import java.util.*;
import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.typechecker.*;
import scalac.util.*;
import Tree.*;

import scalac.transformer.matching.PatternMatcher ;

/** A transformer for expanding match expressions into
 *  flat sequences of .is and .as method calls
 *
 *  @author     Matthias Zenger
 *  @version    1.1
 */
public class TransMatch extends OwnerTransformer {

    public static final Name MATCH_N = Name.fromString("match");

    Unit unit;

    /** type inference engine
     */
    Infer infer;

    public TransMatch(Global global) {
        super(global);
    }

    public void apply(Unit unit) {
        this.unit = unit;
        this.infer = new Infer(this);
        super.apply(unit);
    }

    protected Tree transform(Tree root, CaseDef[] cases, Type restpe) {
        PatternMatcher pm = new PatternMatcher(unit, infer, currentOwner, root, restpe);
        for (int i = 0; i < cases.length; i++)
            pm.enter(cases[i]);
        if (global.log()) {
            global.log("internal pattern matching structure");
            pm.print();
        }
        return pm.toTree();
    }

    public Tree transform(Tree tree) {
        if (tree == null)
            return null;
        switch (tree) {
            case Apply(Select(Tree receiver, MATCH_N), Tree[] args):
                if ((args != null) && (args.length == 1))
                    switch (args[0]) {
                        case Visitor(CaseDef[] cases):
                            return transform(transform(receiver), transform(cases), tree.type);
                    }
                return tree;
            case Apply(TypeApply(Select(Tree receiver, MATCH_N), Tree[] targs), Tree[] args):
                if ((args != null) && (args.length == 1))
                    switch (args[0]) {
                        case Visitor(CaseDef[] cases):
                            return transform(transform(receiver), transform(cases), tree.type);
                    }
                return tree;
            default:
                return super.transform(tree);
        }
    }
}

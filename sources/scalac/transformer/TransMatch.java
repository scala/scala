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
import scalac.util.*;       // Names
import Tree.*;

import scalac.transformer.matching.PatternMatcher ;
import scalac.transformer.matching.TestRegTraverser ;
import scalac.transformer.matching.AlgebraicMatcher ;

/** A transformer for expanding match expressions into
 *  flat sequences of .is and .as method calls
 *
 *  @author     Matthias Zenger, Burak Emir
 *  @version    1.1
 */
public class TransMatch extends OwnerTransformer {

      /** container. classes AlgebraicMatcher and SequenceMatcher get input and store their results in here.
       *  resembles the 'Memento' design pattern, could also be named 'Liaison'
       */
    public static class Matcher {

        /** owner of the code we create (input)
         */
        public Symbol owner;

        /** the selector value (input)
         */
        public Tree   selector;

        /** type of the result of the whole match (input)
         */
        public Type   resultType;

        /** tree representing the matcher (output)
         */
        public Tree   tree;

        public int pos;

        public HashMap varMap;    // needed in LeftTracerInScala
        public Tree[]  stms;      // needed in LeftTracerInScala

        public Matcher(Symbol owner,
                       Tree root,
                       Type resultType) {

            assert( owner != null ) : "owner is null";
            assert owner != Symbol.NONE ;
            this.owner      = owner;

            assert root != null;
            assert root.type != null;
            this.selector   = root;

            assert this.resultType != Type.NoType;
            this.resultType = resultType;

            this.pos        = root.pos; // for convenience only

        }

    }

    Unit unit;

    public TransMatch(Global global) {
        super(global);
    }

    public void apply(Unit unit) {
        this.unit = unit;
        super.apply(unit);
    }

    protected Tree transform(Tree root, CaseDef[] cases, Type restpe) {
        boolean containsReg = false;
        int i = 0;
        while (i < cases.length) {
            containsReg = TestRegTraverser.apply(cases[i]) || containsReg;
            Set nilvars = TestRegTraverser.getNilVariables();
            if(!nilvars.isEmpty()) {
                //System.err.println("nilvars present");
                Tree[] newBody = new Tree[ nilvars.size() + 1 ];
                int j=0;
                for( Iterator it = nilvars.iterator(); it.hasNext(); ) {
                    Symbol v = (Symbol) it.next();
                    newBody[ j++ ] = gen.ValDef(v, gen.mkNil(cases[i].pos));
                }
                newBody[ newBody.length - 1 ] = cases[i].body;
                cases[i].body = gen.mkBlock( newBody );
            }
            i++;
        }
        if (containsReg) {
            AlgebraicMatcher am = new AlgebraicMatcher( unit );
            Matcher matcher = new Matcher( currentOwner, root, restpe );
            am.construct( matcher, cases );
            return matcher.tree;
        } else {
            PatternMatcher pm = new PatternMatcher(unit, root,
                                                   currentOwner, restpe);
            pm.construct(cases);
            if (global.log()) {
                global.log("internal pattern matching structure");
                pm.print();
            }
            return pm.toTree();
        }
    }

    public Tree transform(Tree tree) {
        if (tree == null)
            return null;
        switch (tree) {
            case Apply(Select( Tree receiver, Names.match ), Tree[] args):
                if ((args != null) && (args.length == 1))
                    switch (args[0]) {
                        case Visitor(CaseDef[] cases):
                            return transform(transform(receiver), transform(cases), tree.type);
                    }
                return tree;
            case Apply(TypeApply(Select( Tree receiver, Names.match ), Tree[] targs), Tree[] args):
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

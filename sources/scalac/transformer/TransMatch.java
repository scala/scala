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

	  //public HashMap varMap;
	  //public Tree[]  stms;

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
        PatternMatcher pm = new PatternMatcher( unit, infer );
	Matcher matcher = new Matcher( currentOwner, root, restpe );

	pm.construct( matcher, cases );
        if (global.log()) {
            global.log("internal pattern matching structure");
            pm.print();
        }
        return matcher.tree;
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

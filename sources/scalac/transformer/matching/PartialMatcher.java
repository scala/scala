package scalac.transformer.matching;

import scalac.ast.Tree;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import java.util.HashMap ;

/** container. classes AlgebraicMatcher and SequenceMatcher get input and store their results in here.
 *  resembles the 'Memento' design pattern, could also be named 'Liaison'
 */
public class PartialMatcher {

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

    //public HashMap varMap;    // needed in LeftTracerInScala
    //public Tree[]  stms;      // needed in LeftTracerInScala

    public PartialMatcher(Symbol owner,
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

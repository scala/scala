/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import ch.epfl.lamp.util.Position;
import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.typechecker.*;


/** Intermediate data structure for algebraic + pattern matcher
 */
public class PatternNode {
    public int pos = Position.FIRSTPOS;
    public Type type;
    public PatternNode or;
    public PatternNode and;

    public case Header(Tree selector, Header next) ;
    public case Body(Tree.ValDef[][] bound, Tree[] guard, Tree[] body);
    public case DefaultPat();
    public case ConstrPat(Symbol casted);
    public case ConstantPat(Object value);
    public case VariablePat(Tree tree);
    public case AltPat(Header subheader);
    public case SequencePat(Symbol casted, int len); // only used in PatternMatcher
    public case SeqContainerPat(Symbol casted, Tree seqpat); //   in AlgebraicMatcher

    public PatternNode dup() {
    	PatternNode res;
    	switch (this) {
			case Header(Tree selector, Header next):
				res = Header(selector, next);
				break;
			case Body(Tree.ValDef[][] bound, Tree[] guard, Tree[] body):
				res = Body(bound, guard, body);
				break;
			case DefaultPat():
				res = DefaultPat();
				break;
			case ConstrPat(Symbol casted):
				res = ConstrPat(casted);
				break;
			case SequencePat(Symbol casted, int len):
				res = SequencePat(casted, len);
				break;
			case SeqContainerPat(Symbol casted, Tree seqpat):
				res = SeqContainerPat(casted, seqpat);
				break;
			case ConstantPat(Object value):
				res = ConstantPat(value);
				break;
			case VariablePat(Tree tree):
				res = VariablePat(tree);
				break;
			case AltPat(Header subheader):
				res = AltPat(subheader);
				break;
			default:
				throw new ApplicationError();
    	}
    	res.pos = pos;
   		res.type = type;
   		res.or = or;
   		res.and = and;
   		return res;
    }

    public Symbol symbol() {
        switch (this) {
            case ConstrPat(Symbol casted):
                return casted;
            case SequencePat(Symbol casted, _):
                return casted;
            case SeqContainerPat(Symbol casted, _):
                return casted;
            default:
                return Symbol.NONE;
        }
    }

    public PatternNode next() {
    	switch (this) {
    		case Header(_, Header next):
    			return next;
    		default:
    			return null;
    	}
    }

    public final boolean isDefaultPat() {
        switch( this ) {
        case DefaultPat():
            return true;
        default:
            return false;
        }
    }

    /** returns true if
     *  p and q are equal (constructor | sequence) type tests, or
     *  "q matches" => "p matches"
     */
    public final boolean isSameAs( PatternNode q ) {
        switch( this ) {
        case ConstrPat(_):
            switch (q) {
            case ConstrPat(_):
                return q.type.isSameAs( this.type );
            }
            return false;
        case SequencePat(_, int plen):
            switch (q) {
            case SequencePat(_, int qlen):
                return (plen == qlen) && q.type.isSameAs( this.type );
            }
            return false;
        default:
            return subsumes( q );
        }
    }

    /** returns true if "q matches" => "p matches"
     */
    public final boolean subsumes( PatternNode q ) {
        switch (this) {
        case DefaultPat():
            switch (q) {
            case DefaultPat():
                return true;
            }
            return false;
        case ConstrPat(_):
            switch (q) {
            case ConstrPat(_):
                return q.type.isSubType(this.type);
            }
            return false;
        case SequencePat(_, int plen):
            switch (q) {
            case SequencePat(_, int qlen):
                return (plen == qlen) && q.type.isSubType(this.type);
            }
            return false;
        case ConstantPat(Object pval):
            switch (q) {
            case ConstantPat(Object qval):
                return pval.equals(qval);
            }
            return false;
        case VariablePat(Tree tree):
            switch (q) {
            case VariablePat(Tree other):
                return (tree.symbol() != null) &&
                    (tree.symbol().kind != Kinds.NONE) &&
                    (tree.symbol().kind != Kinds.ERROR) &&
                    (tree.symbol() == other.symbol());
            }
            return false;
        }
        return false;
    }

    public String toString() {
        switch (this) {
            case Header(Tree selector, Header next):
                return "Header(" + selector + ")";
            case Body( _, _, _ ):
                return "Body";
            case DefaultPat():
                return "DefaultPat";
            case ConstrPat(Symbol casted):
                return "ConstrPat(" + casted + ")";
            case SequencePat(Symbol casted, int len):
                return "SequencePat(" + casted + ", " + len + "...)";
            case SeqContainerPat(Symbol casted, Tree seqpat):
                return "SeqContainerPat(" + casted + ", " + seqpat + ")";
            case ConstantPat(Object value):
                return "ConstantPat(" + value + ")";
            case VariablePat(Tree tree):
                return "VariablePat";
            default:
                return "<unknown pat>";
        }
    }
}

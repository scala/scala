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

/** intermediary data structure for algebraic matching
 */

public class PatternNode {
    public int pos = Position.NOPOS;
    public Type type;
    public PatternNode or;
    public PatternNode and;

    public case Header(Tree selector, Header next);
    public case Body(Tree.ValDef[][] bound, Tree[] guard, Tree[] body);
    public case DefaultPat();
    public case ConstrPat(Symbol casted);
    public case SequencePat(Symbol casted, int len, Tree seqpat);
    public case ConstantPat(Object value);
    public case VariablePat(Tree tree);


    public Symbol symbol() {
        switch (this) {
            case ConstrPat(Symbol casted):
                return casted;
            case SequencePat(Symbol casted, _, _):
                return casted;
            default:
                return Symbol.NONE;
        }
    }

    public String toString() {
        switch (this) {
            case Header(Tree selector, Header next):
                return "Header(" + selector + ")";
            case Body(_, _, _):
                return "Body";
            case DefaultPat():
                return "DefaultPat";
            case ConstrPat(Symbol casted):
                return "ConstrPat(" + casted + ")";
            case SequencePat(Symbol casted, int len, Tree seqpat):
                return "SequencePat(" + casted + ", " + len + "...)";
            case ConstantPat(Object value):
                return "ConstantPat(" + value + ")";
            case VariablePat(Tree tree):
                return "VariablePat";
            default:
                return "<unknown pat>";
        }
    }
}

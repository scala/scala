package scalac.transformer.matching;

import ch.epfl.lamp.util.Position;

import scalac.*;
import scalac.ast.*;
import scalac.util.*;
import scalac.symtab.*;
import scalac.typechecker.*;
import PatternNode.*;
import Tree.*;

import java.util.Vector ;

/** PatternNode factory.
 *  we inherit the globals from PatternTool
 */

public class PatternNodeCreator extends PatternTool {

    public SequencePat SequencePat(int pos, Type type, int len, Tree seqpat) {
        Symbol sym = newVar(Position.NOPOS, type);
        SequencePat node = new SequencePat(sym, len, seqpat);
        node.pos = pos;
        node.type = type;
        return node;
    }
    /* MY CODE
       public static SequencePat SequencePat(int pos, Tree seqpat, int case_ix) {
            SequencePat node = new SequencePat( seqpat, case_ix );
            node.pos = pos;
            return node;
      }
    */
      public static DefaultPat DefaultPat(int pos, Type type) {
            DefaultPat node = new DefaultPat();
            node.pos = pos;
            node.type = type;
            return node;
      }

      public ConstrPat ConstrPat(int pos, Type type) {
            ConstrPat node = new ConstrPat(newVar(pos, type));
            node.pos = pos;
            node.type = type;
            return node;
      }

      public static ConstantPat ConstantPat(int pos, Type type, Object value) {
            ConstantPat node = new ConstantPat( value );
            node.pos = pos;
            node.type = type;
            return node;
      }

      public static VariablePat VariablePat(int pos, Tree tree) {
            VariablePat node = new VariablePat( tree );
            node.pos = pos;
            node.type = tree.type;
            return node;
      }

    // factories

    public Header Header(int pos, Type type, Tree selector) {
        Header node = new Header(selector, null);
        node.pos = pos;
        node.type = type;
        return node;
    }

    public Body Body(int pos, ValDef[] bound, Tree guard, Tree body) {
        Body node = new Body(new ValDef[][]{bound}, new Tree[]{guard}, new Tree[]{body});
        node.pos = pos;
        return node;
    }

    public TermSymbol newVar(int pos, Name name, Type type) {
        TermSymbol sym = new TermSymbol(pos, name, owner, 0);
        sym.setType(type);
        //System.out.println("PatternNodeCreator::newVar creates symbol "+sym);
        //System.out.println("owner: "+sym.owner());
        return sym;
    }

    public TermSymbol newVar(int pos, Type type) {
        return newVar(pos, fresh.newName("temp"), type);
    }

      /** the owner of the variable symbols that might be create
       */
      Symbol owner;

      public PatternNodeCreator( Unit unit, Infer infer, Symbol owner ) {
            super( unit, infer );
            assert owner != null;
            this.owner = owner;
      }


}

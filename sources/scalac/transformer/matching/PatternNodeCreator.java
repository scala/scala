package scalac.transformer.matching;

import ch.epfl.lamp.util.Position;

import scalac.*;
import scalac.ast.*;
import scalac.util.*;
import scalac.symtab.*;
import PatternNode.*;
import Tree.*;

import java.util.Vector ;

/** PatternNode factory.
 *  we inherit the globals from PatternTool.
 */

public class PatternNodeCreator extends PatternTool {

    /** the owner of the variable symbols that might be created */
    Symbol owner;

    public PatternNodeCreator(Unit unit, Symbol owner) {
		super(unit);
		assert owner != null;
		this.owner = owner;
    }

    public SequencePat SequencePat(int pos, Type type, int len) {
        Symbol sym = newVar(Position.FIRSTPOS, type);
        SequencePat node = new SequencePat(sym, len);
        node.pos = pos;
        node.type = type;
        return node;
    }

    public SeqContainerPat SeqContainerPat(int pos, Type type, Tree seqpat) {
        Symbol sym = newVar(Position.NOPOS, type);
        SeqContainerPat node = new SeqContainerPat(sym, seqpat);
        node.pos = pos;
        node.type = type;
        return node;
    }

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

  	public static AltPat AltPat(int pos, Header header) {
		AltPat node = new AltPat(header);
		node.pos = pos;
		node.type = header.type;
		return node;
    }

    // factories

    public Header Header(int pos, Type type, Tree selector) {
        Header node = new Header(selector, null);
        node.pos = pos;
        node.type = type;
        return node;
    }

    public Body Body(int pos) {
        Body node = new Body(new ValDef[0][], new Tree[0], new Tree[0]);
        node.pos = pos;
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
}

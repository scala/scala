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
import scalac.util.*;
import scalac.symtab.*;
import scalac.typechecker.*;
import PatternNode.*;
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;
import java.util.Vector ;
import java.util.Iterator ;

public class PatternMatcher extends PatternTool {

    /** the owner of the pattern matching expression
     */
    Symbol owner;

    /** the selector expression
     */
    Tree selector;

    /** the root of the pattern node structure
     */
    PatternNode root;

    /** the symbol of the result variable
     */
    Symbol resultVar;

    /** the statics of class Boolean
     */
    //Symbol statics;

    /** container
     */
    Matcher _m;

    /** methods to generate scala code
     */
    CodeFactory cf;

    /** methods to create pattern nodes
     */
    PatternNodeCreator mk;

    /** constructor
     */
    public PatternMatcher( Unit unit, Infer infer ) {
	super( unit, infer );
    }


    /** constructs an algebraic pattern matcher from cases
     */
    public void construct( Matcher m, Tree[] cases /*, boolean doBinding*/) {
	//this.doBinding = doBinding;
	this._m = m;
	initialize();
	for( int i = 0; i < cases.length; i++ ) {
	    addCase( (CaseDef) cases[i]/*, i*/ );
	}
	_m.tree = toTree();
    }

    /** initializes this AlgebraicMatcher, see Matcher.initialize
     */
    protected void initialize() {

	this.mk = new PatternNodeCreator( unit, infer, _m.owner );
	this.cf = new CodeFactory( unit, infer/*, _m.pos*/ );
	this.owner = _m.owner;
	this.selector = _m.selector;

	this.root = mk.ConstrPat( _m.pos,
				  _m.selector.type.widen() );

	this.root.and = mk.Header( _m.pos,
				   _m.selector.type.widen(),
				   gen.Ident( _m.pos, root.symbol() )
				   .setType(_m.selector.type.widen()));

	this.resultVar = new TermSymbol(_m.pos,
					/*cf.*/fresh.newName( RESULT_N ),
					_m.owner,
					Modifiers.MUTABLE );
	//this.resultType = resultType;
	this.resultVar.setType( _m.resultType );

	//System.out.println(" constructed matcherDefs ");
    }



    /** return the analyzed type
     */
    public Type typeOf(Symbol sym) {
        return sym.type();
        //return sym.typeAt(unit.global.ANALYZER_PHASE.id);
    }

    /** return the analyzed type
     */
    public Type typeOf0(Symbol sym) {
        return sym.typeAt(unit.global.PHASE.ANALYZER.id);
    }

    //
    public void updateBody(Body tree, ValDef[] bound, Tree guard, Tree body) {
        if (tree.guard[tree.guard.length - 1] == Tree.Empty)
            unit.error(body.pos, "unreachable code");
        ValDef[][] bd = new ValDef[tree.bound.length + 1][];
        Tree[] ng = new Tree[tree.guard.length + 1];
        Tree[] nb = new Tree[tree.body.length + 1];
        System.arraycopy(tree.bound, 0, bd, 0, tree.bound.length);
        System.arraycopy(tree.guard, 0, ng, 0, tree.guard.length);
        System.arraycopy(tree.body, 0, nb, 0, tree.body.length);
        bd[bd.length - 1] = bound;
        ng[ng.length - 1] = guard;
        nb[nb.length - 1] = body;
        tree.bound = bd;
        tree.guard = ng;
        tree.body = nb;
    }

    public TermSymbol newVar(int pos, Name name, Type type) {
        TermSymbol sym = new TermSymbol(pos, name, owner, 0);
        sym.setType(type);
        return sym;
    }

    public TermSymbol newVar(int pos, Type type) {
        return newVar(pos, fresh.newName("temp"), type);
    }

    public Tree copy(Tree tree) {
        return tree; // insert copy function here
    }

    /** pretty printer
     */
    public void print() {
        print(root.and, "");
    }

    public void print(PatternNode patNode, String indent) {
        if (patNode == null)
            System.out.println(indent + "NULL");
        else
            switch (patNode) {
                case Header(Tree selector, Header next):
                    System.out.println(indent + "HEADER(" + patNode.type +
                                                ", " + selector + ")");
                    print(patNode.or, indent + "|");
                    if (next != null)
                        print(next, indent);
                    break;
                case ConstrPat(Symbol casted):
                    String s = "-- " + patNode.type.symbol().name +
                               "(" + patNode.type + ", " + casted + ") -> ";
                    String ind = indent;
                    indent = (patNode.or != null) ?
                                    indent :
                                    indent.substring(0, indent.length() - 1) + " ";
                    for (int i = 0; i < s.length(); i++)
                        indent += " ";
                    System.out.println(ind + s);
                    print(patNode.and, indent);
                    if (patNode.or != null)
                        print(patNode.or, ind);
                    break;
                case SequencePat( Symbol casted, int plen ):
                    String s = "-- " + patNode.type.symbol().name + "(" + patNode.type +
                               ", " + casted + ", " + plen + ") -> ";
                    String ind = indent;
                    indent = (patNode.or != null) ?
                                    indent :
                                    indent.substring(0, indent.length() - 1) + " ";
                    for (int i = 0; i < s.length(); i++)
                        indent += " ";
                    System.out.println(ind + s);
                    print(patNode.and, indent);
                    if (patNode.or != null)
                        print(patNode.or, ind);
                    break;
                case DefaultPat():
                    System.out.println(indent + "-- _ -> ");
                    print(patNode.and, indent.substring(0, indent.length() - 1) +
                                        "         ");
                    if (patNode.or != null)
                        print(patNode.or, indent);
                    break;
                case ConstantPat(Object value):
                    String  s = "-- CONST(" + value + ") -> ";
                    String  ind = indent;
                    indent = (patNode.or != null) ?
                                    indent :
                                    indent.substring(0, indent.length() - 1) + " ";
                    for (int i = 0; i < s.length(); i++)
                        indent += " ";
                    System.out.println(ind + s);
                    print(patNode.and, indent);
                    if (patNode.or != null)
                        print(patNode.or, ind);
                    break;
                case VariablePat(Tree tree):
                    String  s = "-- STABLEID(" + tree + ": " + patNode.type + ") -> ";
                    String  ind = indent;
                    indent = (patNode.or != null) ?
                                    indent :
                                    indent.substring(0, indent.length() - 1) + " ";
                    for (int i = 0; i < s.length(); i++)
                        indent += " ";
                    System.out.println(ind + s);
                    print(patNode.and, indent);
                    if (patNode.or != null)
                        print(patNode.or, ind);
                    break;
                case Body(_, _, Tree[] stats):
                    System.out.println(indent + "BODY(" + stats.length + ")");
                    break;
            }
    }

    public void addCase( CaseDef tree ) {
        addCase(tree.pos, tree.pat, tree.guard, tree.body);
    }

    protected CaseEnv addCase(int pos, Tree pat, Tree guard, Tree body) {
        CaseEnv env = new CaseEnv( _m.owner, unit );
        //PatternNode matched = match(pat, root);
        PatternNode target = enter1(pat, -1, root, root.symbol(), env);
        //if (target.and != null)
        //    unit.error(pat.pos, "duplicate case");
        if (target.and == null)
            target.and = mk.Body(pos, env.boundVars(), guard, body);
        else if (target.and instanceof Body)
            updateBody((Body)target.and, env.boundVars(), guard, body);
        else
            unit.error(pat.pos, "duplicate case");
        return env;
    }

    /*

    // unused, thus commented out !

    public PatternNode match(Tree pat, PatternNode target) {
        // advance one step in pattern
        PatternNode next = target.and;
        // we are done (no match yet)
        if (next == null)
            return null;
        // check if matched
        switch (next) {
            case Body(_, _, _):
                return next;
            case Header(_, _):
                Header header = (Header)next;
                // get pattern arguments
                Tree[] patArgs = patternArgs(pat);
                // get next pattern node
                PatternNode patNode = patternNode(pat, header, null);
                do {
                    next = header;
                    while ((next = next.or) != null)
                        if (superPat(next, patNode) &&
                            ((target = match(patArgs, next)) != null))
                            return target;
                        else if (isDefaultPat(next))
                            return next.and;
                } while ((header = header.next) != null);
                return null;
            default:
                throw new ApplicationError();
        }
    }

    public PatternNode match(Tree[] pats, PatternNode target) {
        for (int i = 0; i < pats.length; i++)
            if ((target = match(pats[i], target)) == null)
                return null;
        return target;
    }
    */

    protected Tree[] patternArgs(Tree tree) {
        switch (tree) {
            case Apply(_, Tree[] args):
                if (args.length == 1  && (tree.type.symbol().flags & Modifiers.CASE) == 0)
                    switch (args[0]) {
                        case Sequence(Tree[] ts):
                            return ts;
                    }
                return args;
            case Sequence(Tree[] ts):
		//if( TestRegTraverser.apply( tree ) )
		//  return Tree.EMPTY_ARRAY; // let sequence matcher handle it
            	return ts;
            default:
                return Tree.EMPTY_ARRAY;
        }
    }

    protected Type getConstrType(Type tpe) {
        return tpe;
        /*
        switch (tpe) {
            case ConstructorType(Type result):
                return result;
            default:
                return tpe;
        } */
    }

    protected Type getHeaderType(Type tpe) {
        switch (tpe) {
            case PolyType(_, Type res):
                return res;
            default:
                return tpe;
        }
    }

    protected PatternNode patternNode(Tree tree, Header header, CaseEnv env) {
        switch (tree) {
	case Apply(Tree fn, Tree[] args):             // pattern with args
	    if (args.length == 1 && (tree.type.symbol().flags & Modifiers.CASE) == 0)
		switch (args[0]) {
		case Sequence(Tree[] ts):
		    return mk.SequencePat( tree.pos, tree.type, ts.length );
		}
	    return mk.ConstrPat(tree.pos, getConstrType(tree.type));
	case Typed(Ident(Name name), Tree tpe):       // variable pattern
	    PatternNode node =
		(header.type.isSubType(getConstrType(tpe.type))) ?
		mk.DefaultPat(tree.pos, getConstrType(tpe.type))
		: mk.ConstrPat(tree.pos, getConstrType(tpe.type));
	    if ((env != null) && (name != Names.WILDCARD))
		switch (node) {
		case ConstrPat(Symbol casted):
		    env.newBoundVar(
				    tree.pos,
				    ((Tree.Typed)tree).expr.symbol(),
				    getConstrType(tpe.type),
				    make.Ident(tree.pos, casted.name).
				    setType(typeOf(casted)).
				    setSymbol(casted));
		    break;
		default:
		    env.newBoundVar(
				    tree.pos,
				    ((Tree.Typed)tree).expr.symbol(),
				    getConstrType(tpe.type),
				    header.selector);
		}
	    return node;
	case Ident(Name name):        		// pattern without args or variable
	    if (tree.symbol().isPrimaryConstructor())
		return mk.ConstrPat(tree.pos, getConstrType(tree.type));
	    else if (name.isVariable()) {
		if ((env != null) && (name != Names.WILDCARD))
		    env.newBoundVar(
				    tree.pos,
				    tree.symbol(),
				    getConstrType(tree.type),
				    header.selector);
		return mk.DefaultPat(tree.pos, getConstrType(header.type));
	    } else
		return mk.VariablePat(tree.pos, tree);
	case Select(_, Name name):                                    // variable
	    if (tree.symbol().isPrimaryConstructor())
		return mk.ConstrPat(tree.pos, getConstrType(tree.type));
	    else
		return mk.VariablePat(tree.pos, tree);
	case Literal(Object value):
	    return mk.ConstantPat(tree.pos, getConstrType(tree.type), value);
	case Sequence(Tree[] ts):
	    return mk.SequencePat(tree.pos, tree.type, ts.length);
	case Alternative(Tree[] ts): // CAN THIS WORK ?
	    assert ts.length > 0;
	    PatternNode res = patternNode( ts[ 0 ], header, env );
	    for( int i = 1; i<ts.length; i++ ) {
		res.or = patternNode( ts[ i ], header, env );
		res = res.or ;
	    }
	    return res;
	default:
	    new scalac.ast.printer.TextTreePrinter().print(tree).flush();
	    throw new ApplicationError(tree);
        }
    }

    protected boolean superPat(PatternNode p, PatternNode q) {
        switch (p) {
            case DefaultPat():
                switch (q) {
                    case DefaultPat():
                        return true;
                    //case ConstantPat(_, _):
                    //    return q.type.isSubType(p.type);
                }
                return false;
            case ConstrPat(_):
                switch (q) {
                    case ConstrPat(_):
                        return q.type.isSubType(p.type);
                }
                return false;
            case SequencePat(_, int plen):
                switch (q) {
                    case SequencePat(_, int qlen):
                        return (plen == qlen) && q.type.isSubType(p.type);
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

    protected boolean isDefaultPat(PatternNode p) {
        switch (p) {
            case DefaultPat():
                return true;
            default:
                return false;
        }
    }

    public PatternNode enter(Tree pat,
                             int index,
                             PatternNode target,
                             Symbol casted,
                             CaseEnv env) {
        switch (target) {
            case ConstrPat(Symbol newCasted):
                return enter1(pat, index, target, newCasted, env);
            case SequencePat(Symbol newCasted, int len):
                return enter1(pat, index, target, newCasted, env);
            default:
                return enter1(pat, index, target, casted, env);
        }
    }

    public PatternNode enter1(Tree pat,
                             int index,
                             PatternNode target,
                             Symbol casted,
                             CaseEnv env) {
        //System.out.println("enter(" + pat + ", " + index + ", " + target + ", " + casted + ")");
        // get pattern arguments
        Tree[] patArgs = patternArgs(pat);
        // get case fields
        //assert patArgs.length == nCaseComponents(pat);
        // advance one step in pattern
        Header curHeader = (Header)target.and;
        // check if we have to add a new header
        if (curHeader == null) {
            assert index >= 0 : casted;
            if (casted.pos == Position.FIRSTPOS) {
                Symbol atSym = casted.type().lookup(APPLY_N);
                //System.out.println("casted type = " + typeOf(casted));
                Type seqType = casted.type().baseType(defs.SEQ_CLASS).typeArgs()[0];
                Tree t = make.Select(
                            pat.pos,
                            make.Ident(pat.pos, casted.name)
                                .setType(typeOf(casted))
                                .setSymbol(casted),
                            APPLY_N);
                switch (typeOf(atSym)) {
                    case OverloadedType(Symbol[] alts, Type[] alttypes):
                        infer.methodAlternative(t, alts, alttypes,
                            new Type[]{defs.INT_TYPE}, seqType);
                        t = make.Apply(pat.pos, t,
                                new Tree[]{
                                    make.Literal(pat.pos, new Integer(index))
                                        .setType(defs.INT_TYPE)
                                }).setType(seqType);
                        break;
                    default:
                        t.setSymbol(atSym);
                        t.setType(typeOf(atSym));
                        t = make.Apply(pat.pos, t,
                                new Tree[]{
                                    make.Literal(pat.pos, new Integer(index))
                                        .setType(defs.INT_TYPE)
                                }).setType(seqType);
                }
                target.and = curHeader = mk.Header(pat.pos, seqType, t);
            } else {
                Symbol ts = ((ClassSymbol) casted.type().symbol())
                    .caseFieldAccessor(index);
//                 target.and = curHeader = mk.Header(
//                     pat.pos,
//                     getHeaderType(typeOf0(ts)),
//                     make.Select(
//                             pat.pos,
//                             make.Ident(pat.pos, casted.name)
//                                 .setType(typeOf(casted))
//                                 .setSymbol(casted),
//                             ts.name)
//                             .setType(getHeaderType(typeOf0(ts)))
//                             .setSymbol(ts));
                target.and = curHeader = mk.Header(
                    pat.pos,
                    getHeaderType(typeOf0(ts)),
                    make.Apply(
                        pat.pos,
                        make.Select(
                            pat.pos,
                            make.Ident(pat.pos, casted.name)
                                .setType(typeOf(casted))
                                .setSymbol(casted),
                            ts.name)
                            .setType(Type.MethodType(
                                      Symbol.EMPTY_ARRAY,
                                      getHeaderType(typeOf0(ts))))
                            .setSymbol(ts),
                        Tree.EMPTY_ARRAY).setType(getHeaderType(typeOf0(ts)).asSeenFrom(typeOf(casted), ts.owner())));
            }
            curHeader.or = patternNode(pat, curHeader, env);
            return enter(patArgs, curHeader.or, casted, env);
        }
        // find most recent header
        while (curHeader.next != null)
            curHeader = curHeader.next;
         // create node
        PatternNode patNode = patternNode(pat, curHeader, env);
        PatternNode next = curHeader;
        // enter node
        while (true)
            if (superPat(next, patNode))
                return enter(patArgs, next, casted, env);
            else if (isDefaultPat(next) ||
                     ((next.or == null) && isDefaultPat(patNode)))
                return enter(
                    patArgs,
                    (curHeader = (curHeader.next =
                        mk.Header(patNode.pos, curHeader.type, curHeader.selector))).or
                            = patNode,
                    casted,
                    env);
            else if (next.or == null)
                return enter(patArgs, next.or = patNode, casted, env);
            else
                next = next.or;
    }

    public PatternNode enter(Tree[] pats, PatternNode target, Symbol casted, CaseEnv env) {
        switch (target) {
            case ConstrPat(Symbol newCasted):
                casted = newCasted;
                break;
            case SequencePat(Symbol newCasted, int len):
                casted = newCasted;
                break;
        }
        for (int i = 0; i < pats.length; i++)
            target = enter1(pats[i], i, target, casted, env);
        return target;
    }

    protected int nCaseComponents(Tree tree) {
        switch (tree) {
            case Apply(Tree fn, _):
                Type tpe = typeOf(tree.type.symbol().primaryConstructor());
                //System.out.println("~~~ " + tree.type() + ", " + tree.type().symbol().primaryConstructor());
                switch (tpe) {
                    // I'm not sure if this is a good idea, but obviously, currently all case classes
                    // without constructor arguments have type NoType
                    case NoType:
                        return 0;
                    case MethodType(Symbol[] args, _):
                        return args.length;
                    case PolyType(Symbol[] tvars, MethodType(Symbol[] args, _)):
                        return args.length;
                    case PolyType(Symbol[] tvars, _):
                        return 0;
                    default:
                        throw new ApplicationError("not yet implemented;" +
                            "pattern matching for " + tree + ": " + tpe);
                }
        }
        return 0;
    }


//////////// generator methods

    public Tree toTree() {
        TreeList ts = new TreeList();
        ts.append(
            make.ValDef(selector.pos,
                        0,
                        root.symbol().name,
                        gen.mkType(selector.pos, selector.type),
                        selector).setType(defs.UNIT_TYPE).setSymbol(root.symbol()));
        ts.append(
            make.ValDef(selector.pos,
                        Modifiers.MUTABLE,
                        resultVar.name,
                        gen.mkType(selector.pos, typeOf(resultVar)),
                        Tree.Empty).setType(defs.UNIT_TYPE).setSymbol(resultVar));
        ts.append(
            make.If(
                selector.pos,
                toTree(root.and),
                make.Ident(selector.pos,
                           resultVar.name).setType(typeOf(resultVar)).setSymbol(resultVar),
                cf.ThrowMatchError(selector.pos, typeOf(resultVar))).setType(typeOf(resultVar)));
        return cf.Block(selector.pos, ts.toArray(), typeOf(resultVar));
    }

    protected Tree toTree(PatternNode node) {
        Tree res = gen.mkBooleanLit(node.pos, false);
        while (node != null)
            switch (node) {
                case Header(Tree selector, Header next):
                    //res = cf.And(mkNegate(res), toTree(node.or, selector));
                    //System.out.println("HEADER TYPE = " + selector.type);
                    res = cf.Or(res, toTree(node.or, selector));
                    node = next;
                    break;
                case Body(ValDef[][] bound, Tree[] guard, Tree[] body):
                    for (int i = guard.length - 1; i >= 0; i--) {
                        Tree[] ts = new Tree[bound[i].length + 1];
                        System.arraycopy(bound[i], 0, ts, 0, bound[i].length);
                        ts[bound[i].length] = cf.Block(body[i].pos,
                            new Tree[]{
                                make.Assign(
                                    body[i].pos,
                                    make.Ident(body[i].pos, resultVar.name)
                                        .setType(typeOf(resultVar)).setSymbol(resultVar),
                                    body[i]).setType(defs.UNIT_TYPE),
                                gen.mkBooleanLit(body[i].pos, true)
                            }, defs.BOOLEAN_TYPE);
                        if (guard[i] != Tree.Empty)
                            ts[bound[i].length] = cf.And(guard[i], ts[bound[i].length]);
                        res = cf.Or(cf.Block(body[i].pos, ts, defs.BOOLEAN_TYPE), res);
                    }
                    return res;
                default:
                    throw new ApplicationError();
            }
        return res;
    }

    protected Tree toTree(PatternNode node, Tree selector) {
        if (node == null)
            return gen.mkBooleanLit(selector.pos, false);
        switch (node) {
            case DefaultPat():
                return toTree(node.and);
            case ConstrPat(Symbol casted):
                return make.If(
                        selector.pos,
                        cf.Is(selector.duplicate(), node.type),
                        cf.Block(selector.pos,
                            new Tree[]{
                                make.ValDef(selector.pos,
                                            0,
                                            casted.name,
                                            gen.mkType(selector.pos, node.type),
                                            cf.As(selector.duplicate(), node.type))
                                    .setType(defs.UNIT_TYPE).setSymbol(casted),
                                toTree(node.and)}, defs.BOOLEAN_TYPE),
                        toTree(node.or, selector.duplicate())).setType(defs.BOOLEAN_TYPE);
            case SequencePat(Symbol casted, int len):
                Symbol lenSym = casted.type().lookup(LENGTH_N);
                Tree t = make.Select(selector.pos, cf.As(selector.duplicate(), node.type), LENGTH_N);
                switch (typeOf(lenSym)) {
                    case OverloadedType(Symbol[] alts, Type[] alttypes):
                        infer.methodAlternative(t, alts, alttypes, new Type[0], defs.INT_TYPE);
                        break;
                    default:
                        t.setSymbol(lenSym);
                        t.setType(typeOf(lenSym));
                }
                return make.If(
                        selector.pos,
                        cf.And(
                            cf.Is(selector.duplicate(), node.type),
                            cf.Equals(
                                make.Apply(
                                    selector.pos, t,
                                    Tree.EMPTY_ARRAY).setType(defs.INT_TYPE),
                                make.Literal(selector.pos, new Integer(len))
                            .setType(defs.INT_TYPE))),
                        cf.Block(selector.pos,
                            new Tree[]{
                                make.ValDef(selector.pos,
                                            0,
                                            casted.name,
                                            gen.mkType(selector.pos, node.type),
                                            cf.As(selector.duplicate(), node.type))
                                    .setType(defs.UNIT_TYPE).setSymbol(casted),
                                toTree(node.and)}, defs.BOOLEAN_TYPE),
                        toTree(node.or, selector.duplicate()))
                            .setType(defs.BOOLEAN_TYPE);

            case ConstantPat(Object value):
                return make.If(
                        selector.pos,
                        cf.Equals(selector.duplicate(),
                            make.Literal(selector.pos, value)
                                .setType(node.type)),
                        toTree(node.and),
                        toTree(node.or, selector.duplicate())).setType(defs.BOOLEAN_TYPE);
            case VariablePat(Tree tree):
                return make.If(
                        selector.pos,
                        cf.Equals(selector.duplicate(), tree),
                        toTree(node.and),
                        toTree(node.or, selector.duplicate())).setType(defs.BOOLEAN_TYPE);
            default:
                throw new ApplicationError();
        }
    }

}

/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer;

import scalac.*;
import scalac.ast.*;
import scalac.util.*;
import scalac.symtab.*;
import scalac.typechecker.*;
import PatternNode.*;
import Tree.*;


public class PatternMatcher {

    public static final Name RESULT_N = Name.fromString("$result");
    public static final Name SCALA_N = Name.fromString("scala");
    public static final Name BOOLEAN_N = Name.fromString("Boolean");
    public static final Name TRUE_N = Name.fromString("True");
    public static final Name FALSE_N = Name.fromString("False");
    public static final Name AND_N = Name.fromString("$amp$amp");
    public static final Name OR_N = Name.fromString("$bar$bar");
    public static final Name NOT_N = Name.fromString("$bang");
    public static final Name EQUALS_N = Name.fromString("$eq$eq");
    public static final Name SCALA_MATCHERROR_N = Name.fromString("scala.MatchError");
    public static final Name MATCHERROR_N = Name.fromString("MatchError");
    public static final Name FAIL_N = Name.fromString("fail");
    public static final Name WILDCARD_N = Name.fromString("_");
    public static final Name LENGTH_N = Name.fromString("length");
    public static final Name AT_N = Name.fromString("at");

    /** the current compilation unit
     */
    Unit unit;

    /** the global fresh name creator
     */
    FreshNameCreator fresh;

    /** the global tree factory
     */
    TreeFactory make;

    /** the global definitions component
     */
    Definitions defs;

    /** the global tree generation component
     */
    TreeGen gen;

    /** type inference engine
     */
    Infer infer;

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
    Symbol statics;

    /** true symbol
     */
    Symbol trueSym;

    /** false symbol
     */
    Symbol falseSym;


    /** constructor
     */
    public PatternMatcher(Unit unit, Infer infer, Symbol owner, Tree selector, Type tpe) {
        this.unit = unit;
        this.fresh = unit.global.freshNameCreator;
        this.make = unit.global.make;
        this.gen = unit.global.treeGen;
        this.defs = unit.global.definitions;
        this.owner = owner;
        this.selector = selector;
        this.root = makeConstrPat(selector.pos,
                                  selector.type);
        this.root.and = makeHeader(selector.pos,
                                   selector.type,
                                   make.Ident(selector.pos,
                                              root.symbol().name)
                                    .setType(selector.type)
                                    .setSymbol(root.symbol()));
        this.resultVar = new TermSymbol(selector.pos,
                                        RESULT_N,
                                        owner,
                                        Modifiers.MUTABLE);
        this.resultVar.setType(tpe);
        this.statics = defs.getModule(Names.scala_Boolean);
        this.trueSym = defs.TRUE();
        this.falseSym = defs.FALSE();
        this.infer = infer;
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

    /** factories
     */
    public Header makeHeader(int pos, Type type, Tree selector) {
        Header node = new Header(selector, null);
        node.pos = pos;
        node.type = type;
        return node;
    }

    public Body makeBody(int pos, ValDef[] bound, Tree guard, Tree body) {
        Body node = new Body(new ValDef[][]{bound}, new Tree[]{guard}, new Tree[]{body});
        node.pos = pos;
        return node;
    }

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

    public DefaultPat makeDefaultPat(int pos, Type type) {
        DefaultPat node = new DefaultPat();
        node.pos = pos;
        node.type = type;
        return node;
    }

    public ConstrPat makeConstrPat(int pos, Type type) {
        ConstrPat node = new ConstrPat(newVar(pos, type));
        node.pos = pos;
        node.type = type;
        return node;
    }

    public SequencePat makeSequencePat(int pos, Type type, int len) {
        Symbol sym = newVar(Position.NOPOS, type);
        SequencePat node = new SequencePat(sym, len);
        node.pos = pos;
        node.type = type;
        return node;
    }

    public ConstantPat makeConstantPat(int pos, Type type, Object value) {
        ConstantPat node = new ConstantPat(value);
        node.pos = pos;
        node.type = type;
        return node;
    }

    public VariablePat makeVariablePat(int pos, Tree tree) {
        VariablePat node = new VariablePat(tree);
        node.pos = pos;
        node.type = tree.type;
        return node;
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
                    System.out.println(indent + "HEADER(" + patNode.type + ", " + selector + ")");
                    print(patNode.or, indent + "|");
                    if (next != null)
                        print(next, indent);
                    break;
                case ConstrPat(Symbol casted):
                    String s = "-- " + patNode.type.symbol().name + "(" + patNode.type + ", " + casted + ") -> ";
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
                case SequencePat(Symbol casted, int plen):
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

    public void enter(CaseDef tree) {
        enter(tree.pos, tree.pat, tree.guard, tree.body);
    }

    protected CaseEnv enter(int pos, Tree pat, Tree guard, Tree body) {
        CaseEnv env = new CaseEnv();
        //PatternNode matched = match(pat, root);
        PatternNode target = enter1(pat, -1, root, root.symbol(), env);
        //if (target.and != null)
        //    unit.error(pat.pos, "duplicate case");
        if (target.and == null)
            target.and = makeBody(pos, env.boundVars(), guard, body);
        else if (target.and instanceof Body)
            updateBody((Body)target.and, env.boundVars(), guard, body);
        else
            unit.error(pat.pos, "duplicate case");
        return env;
    }

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

    protected Tree[] patternArgs(Tree tree) {
        switch (tree) {
            case Apply(_, Tree[] args):
                if (args.length == 1)
                    switch (args[0]) {
                        case Tuple(Tree[] ts):
                            return ts;
                    }
                return args;
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
                if (args.length == 1)
                    switch (args[0]) {
                        case Tuple(Tree[] ts):
                            return makeSequencePat(tree.pos, tree.type, ts.length);
                    }
                return makeConstrPat(tree.pos, getConstrType(tree.type));
            case Typed(Ident(Name name), Tree tpe):       // variable pattern
                if ((env != null) && (name != WILDCARD_N))
                    env.newBoundVar(tree.pos, ((Tree.Typed)tree).expr.symbol(), getConstrType(tpe.type), header);
                if (header.type.isSubType(getConstrType(tpe.type)))
                    return makeDefaultPat(tree.pos, getConstrType(tpe.type));
                else
                    return makeConstrPat(tree.pos, getConstrType(tpe.type));
            case Ident(Name name):        // pattern without args or variable
                if (tree.symbol().isPrimaryConstructor())
                    return makeConstrPat(tree.pos, getConstrType(tree.type));
                else if (name.isVariable()) {
                    if ((env != null) && (name != WILDCARD_N))
                        env.newBoundVar(tree.pos, tree.symbol(), getConstrType(tree.type), header);
                    return makeDefaultPat(tree.pos, getConstrType(header.type));
                } else
                    return makeVariablePat(tree.pos, tree);
            case Select(_, Name name):                                    // variable
                if (tree.symbol().isPrimaryConstructor())
                    return makeConstrPat(tree.pos, getConstrType(tree.type));
                else
                    return makeVariablePat(tree.pos, tree);
            case Literal(Object value):
                return makeConstantPat(tree.pos, getConstrType(tree.type), value);
            default:
                throw new ApplicationError();
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
            if (casted.pos == Position.NOPOS) {
                Symbol atSym = casted.type().lookup(AT_N);
                //System.out.println("casted type = " + typeOf(casted));
                Type seqType = casted.type().baseType(defs.SEQ_CLASS).typeArgs()[0];
                Tree t = make.Select(
                            pat.pos,
                            make.Ident(pat.pos, casted.name)
                                .setType(typeOf(casted))
                                .setSymbol(casted),
                            AT_N);
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
                target.and = curHeader = makeHeader(pat.pos, seqType, t);
            } else {
                Symbol ts = ((ClassSymbol) casted.type().symbol())
                    .caseFieldAccessor(index);
                target.and = curHeader = makeHeader(
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
                        Tree.EMPTY_ARRAY).setType(getHeaderType(typeOf0(ts))));
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
                        makeHeader(patNode.pos, curHeader.type, curHeader.selector))).or
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
                Type tpe = typeOf(tree.type.symbol().constructor());
                //System.out.println("~~~ " + tree.type() + ", " + tree.type().symbol().constructor());
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
                mkThrowMatchError(selector.pos, typeOf(resultVar))).setType(typeOf(resultVar)));
        return mkBlock(selector.pos, ts.toArray(), typeOf(resultVar));
    }

    protected Tree toTree(PatternNode node) {
        Tree res = mkBoolean(node.pos, false);
        while (node != null)
            switch (node) {
                case Header(Tree selector, Header next):
                    //res = mkAnd(mkNegate(res), toTree(node.or, selector));
                    res = mkOr(res, toTree(node.or, selector));
                    node = next;
                    break;
                case Body(ValDef[][] bound, Tree[] guard, Tree[] body):
                    for (int i = guard.length - 1; i >= 0; i--) {
                        Tree[] ts = new Tree[bound[i].length + 1];
                        System.arraycopy(bound[i], 0, ts, 0, bound[i].length);
                        ts[bound[i].length] = mkBlock(body[i].pos,
                            new Tree[]{
                                make.Assign(
                                    body[i].pos,
                                    make.Ident(body[i].pos, resultVar.name)
                                        .setType(typeOf(resultVar)).setSymbol(resultVar),
                                    body[i]).setType(defs.UNIT_TYPE),
                                mkBoolean(body[i].pos, true)
                            }, defs.BOOLEAN_TYPE);
                        if (guard[i] != Tree.Empty)
                            ts[bound[i].length] = mkAnd(guard[i], ts[bound[i].length]);
                        res = mkOr(mkBlock(body[i].pos, ts, defs.BOOLEAN_TYPE), res);
                    }
                    return res;
                default:
                    throw new ApplicationError();
            }
        return res;
    }

    protected Tree toTree(PatternNode node, Tree selector) {
        if (node == null)
            return mkBoolean(selector.pos, false);
        switch (node) {
            case DefaultPat():
                return toTree(node.and);
            case ConstrPat(Symbol casted):
                return make.If(
                        selector.pos,
                        mkIs(selector, node.type),
                        mkBlock(selector.pos,
                            new Tree[]{
                                make.ValDef(selector.pos,
                                            0,
                                            casted.name,
                                            gen.mkType(selector.pos, node.type),
                                            mkAs(selector, node.type))
                                    .setType(defs.UNIT_TYPE).setSymbol(casted),
                                toTree(node.and)}, defs.BOOLEAN_TYPE),
                        toTree(node.or, selector)).setType(defs.BOOLEAN_TYPE);
            case SequencePat(Symbol casted, int len):
                Symbol lenSym = casted.type().lookup(LENGTH_N);
                Tree t = make.Select(selector.pos, mkAs(selector, node.type), LENGTH_N);
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
                        mkAnd(
                            mkIs(selector, node.type),
                            mkEquals(
                                make.Apply(
                                    selector.pos, t,
                                    Tree.EMPTY_ARRAY).setType(defs.INT_TYPE),
                                make.Literal(selector.pos, new Integer(len))
                            .setType(defs.INT_TYPE))),
                        mkBlock(selector.pos,
                            new Tree[]{
                                make.ValDef(selector.pos,
                                            0,
                                            casted.name,
                                            gen.mkType(selector.pos, node.type),
                                            mkAs(selector, node.type))
                                    .setType(defs.UNIT_TYPE).setSymbol(casted),
                                toTree(node.and)}, defs.BOOLEAN_TYPE),
                        toTree(node.or, selector))
                            .setType(defs.BOOLEAN_TYPE);
            case ConstantPat(Object value):
                return make.If(
                        selector.pos,
                        mkEquals(selector,
                            make.Literal(selector.pos, value)
                                .setType(node.type)),
                        toTree(node.and),
                        toTree(node.or, selector)).setType(defs.BOOLEAN_TYPE);
            case VariablePat(Tree tree):
                return make.If(
                        selector.pos,
                        mkEquals(selector, tree),
                        toTree(node.and),
                        toTree(node.or, selector)).setType(defs.BOOLEAN_TYPE);
            default:
                throw new ApplicationError();
        }
    }

    protected Tree mkBlock(int pos, Tree[] ts, Type tpe) {
        if (ts.length == 1)
            return ts[0];
        else if (ts.length > 1)
            switch (ts[ts.length - 1]) {
                case Block(Tree[] ts0):
                    Tree[] ts1 = new Tree[ts0.length + ts.length - 1];
                    System.arraycopy(ts, 0, ts1, 0, ts.length - 1);
                    System.arraycopy(ts0, 0, ts1, ts.length - 1, ts0.length);
                    return mkBlock(pos, ts1, tpe);
            }
        return make.Block(pos, ts).setType(tpe);
    }

    protected Tree mkBoolean(int pos, boolean bool) {
        Name name = bool ? TRUE_N : FALSE_N;
        Symbol val = bool ? trueSym : falseSym;
        return make.Select(
                    pos,
                    make.Select(
                        pos,
                        make.Ident(pos, SCALA_N)
                            .setType(typeOf(defs.SCALA)).setSymbol(defs.SCALA),
                        BOOLEAN_N).setType(typeOf(statics)).setSymbol(statics),
                    name).setType(defs.BOOLEAN_TYPE).setSymbol(val);
    }

    protected Tree mkNegate(Tree tree) {
        switch (tree) {
            case Select(_, _):
                if (tree.symbol() == trueSym)
                    return mkBoolean(tree.pos, false);
                else if (tree.symbol() == falseSym)
                    return mkBoolean(tree.pos, true);
        }
        Symbol fun = tree.type.lookup(NOT_N);
        if (fun.name == NOT_N) {
            //System.out.println(fun.typeAt(unit.global.UNCURRY_PHASE.id));//DEBUG
            //System.out.println(fun.typeAt(unit.global.TRANSMATCH_PHASE.id));//DEBUG
            //System.out.println(fun.type());//DEBUG
        }
        return make.Apply(
                tree.pos,
/*
                make.Select(
                    tree.pos,
                    tree,
                    NOT_N).type(typeOf(fun)).symbol(fun),
*/
                gen.Select(tree, NOT_N),
                Tree.EMPTY_ARRAY).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree mkAnd(Tree left, Tree right) {
        switch (left) {
            case Select(_, _):
                if (left.symbol() == trueSym)
                    return right;
                else if (left.symbol() == falseSym)
                    return left;
        }
        switch (right) {
            case Select(_, _):
                if (right.symbol() == trueSym)
                    return left;
        }
        Symbol fun = left.type.lookup(AND_N);
        return make.Apply(
                left.pos,
                make.Select(
                    left.pos,
                    left,
                    AND_N).setType(typeOf(fun)).setSymbol(fun),
                new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree mkOr(Tree left, Tree right) {
        switch (left) {
            case Select(_, _):
                if (left.symbol() == trueSym)
                    return left;
                else if (left.symbol() == falseSym)
                    return right;
        }
        switch (right) {
            case Select(_, _):
                if (right.symbol() == falseSym)
                    return left;
        }
        Symbol fun = left.type.lookup(OR_N);
        return make.Apply(
                left.pos,
                make.Select(
                    left.pos,
                    left,
                    OR_N).setType(typeOf(fun)).setSymbol(fun),
                new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree mkIs(Tree tree, Type type) {
        return
            make.Apply(
                tree.pos,
                make.TypeApply(
                    tree.pos,
                    make.Select(
                        tree.pos,
                        tree,
                        defs.IS.name).setType(typeOf(defs.IS)).setSymbol(defs.IS),
                    new Tree[]{gen.mkType(tree.pos, type)})
                .setType(Type.MethodType(Symbol.EMPTY_ARRAY, defs.BOOLEAN_TYPE)),
                Tree.EMPTY_ARRAY).setType(defs.BOOLEAN_TYPE);
    }

    protected Tree mkAs(Tree tree, Type type) {
        return
            make.Apply(
                tree.pos,
                make.TypeApply(
                    tree.pos,
                    make.Select(
                        tree.pos,
                        tree,
                        defs.AS.name).setType(typeOf(defs.AS)).setSymbol(defs.AS),
                    new Tree[]{gen.mkType(tree.pos, type)})
                .setType(Type.MethodType(Symbol.EMPTY_ARRAY, type)),
                Tree.EMPTY_ARRAY).setType(type);
    }

    protected Tree mkEquals(Tree left, Tree right) {
        Symbol fun = left.type.lookup(EQUALS_N);
        switch (typeOf(fun)) {
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                Tree t = make.Select(left.pos, left, EQUALS_N);
                infer.methodAlternative(t, alts, alttypes,
                    new Type[]{right.type}, defs.BOOLEAN_TYPE);
                return make.Apply(left.pos, t, new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
            default:
                return make.Apply(
                    left.pos,
                    make.Select(
                        left.pos,
                        left,
                        EQUALS_N).setType(typeOf(fun)).setSymbol(fun),
                    new Tree[]{right}).setType(defs.BOOLEAN_TYPE);
        }
    }

    protected Tree mkThrowMatchError(int pos, Type type) {
        Symbol matchErrorModule = defs.SCALA.members().lookup(MATCHERROR_N);
        outer: switch (typeOf(matchErrorModule)) {
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                for (int i = 0; i < alts.length; i++)
                    switch (alttypes[i]) {
                        case TypeRef(_, _, _):
                            matchErrorModule = alts[i];
                            break outer;
                    }
        }
        Symbol failMethod = typeOf(matchErrorModule).lookup(FAIL_N);
        return
        make.Apply(
            pos,
            make.TypeApply(
               pos,
               make.Select(
                pos,
                make.Select(
                    pos,
                    make.Ident(pos, Names.scala).setType(typeOf(defs.SCALA)).setSymbol(defs.SCALA),
                    MATCHERROR_N)
                   .setSymbol(matchErrorModule)
                   .setType(typeOf(matchErrorModule)),
                FAIL_N).setType(typeOf(failMethod)).setSymbol(failMethod),
                new Tree[]{gen.mkType(pos, type)})
              .setType(((Type.PolyType) typeOf(failMethod)).result.subst(
                    typeOf(failMethod).typeParams(),
                    new Type[]{type})),
            new Tree[]{
                make.Literal(pos, unit.toString()).setType(defs.STRING_TYPE),
                make.Literal(pos, new Integer(Position.line(pos))).setType(defs.INT_TYPE)
            }).setType(type);
    }

    class CaseEnv {
        protected ValDef[] boundVars = new ValDef[4];
        protected int numVars = 0;

        public void newBoundVar(int pos, Symbol sym, Type type, Header sh) {
            sym.setOwner(PatternMatcher.this.owner); // FIXME should be corrected earlier
            if (numVars == boundVars.length) {
                ValDef[] newVars = new ValDef[numVars * 2];
                System.arraycopy(boundVars, 0, newVars, 0, numVars);
                boundVars = newVars;
            }
            sym.setType(type);
            boundVars[numVars++] = (ValDef)make.ValDef(
                pos,
                0,
                sym.name,
                gen.mkType(pos, type),
                sh.selector).setType(defs.UNIT_TYPE).setSymbol(sym);
        }

        public ValDef[] boundVars() {
            ValDef[] newVars = new ValDef[numVars];
            System.arraycopy(boundVars, 0, newVars, 0, numVars);
            return newVars;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof CaseEnv))
                return false;
            CaseEnv env = (CaseEnv)obj;
            if (env.numVars != numVars)
                return false;
            for (int i = 0; i < numVars; i++)
                if ((boundVars[i].name != env.boundVars[i].name) ||
                    !boundVars[i].tpe.type.isSameAs(env.boundVars[i].tpe.type) ||
                    (boundVars[i].rhs != env.boundVars[i].rhs))
                    return false;
            return true;
        }
    }
}

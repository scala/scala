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
import scalac.atree.AConstant;
import scalac.util.*;
import scalac.symtab.*;
import PatternNode.*;
import Tree.*;


public class PatternMatcher extends PatternTool {

      protected boolean optimize = true;
    protected boolean delegateSequenceMatching = false;
    protected boolean doBinding = true;

    /** the owner of the pattern matching expression
     */
    protected Symbol owner;

    /** the selector expression
     */
    protected Tree selector;

    /** the root of the pattern node structure
     */
    protected PatternNode root;

    /** the symbol of the result variable
     */
    protected Symbol resultVar;

    /** methods to generate scala code
     */
    protected CodeFactory cf;

    /** methods to create pattern nodes
     */
    protected PatternNodeCreator mk;

    /** constructor
     */
    public PatternMatcher(Unit unit, Tree selector,
                          Symbol owner, Type resultType) {
        super(unit);
        initialize(selector, owner, resultType, true);
    }

    /** constructor, used in subclass ALgebraicMatcher
     */
    protected PatternMatcher(Unit unit) {
        super(unit);
    }

    /** init method, also needed in subclass AlgebraicMatcher
     */
    protected void initialize(Tree selector, Symbol owner, Type resultType,  boolean doBinding) {
        this.mk = new PatternNodeCreator(unit, owner);
        this.cf = new CodeFactory(unit, selector.pos);
        this.root = mk.ConstrPat(selector.pos, selector.type.widen());
        this.root.and = mk.Header(selector.pos,
                                  selector.type.widen(),
                                  gen.Ident(selector.pos, root.symbol()));
        this.resultVar = new TermSymbol(selector.pos,
                                        fresh.newName(RESULT_N),
                                        owner,
                                        Modifiers.MUTABLE);
        this.resultVar.setType(resultType);
        this.owner = owner;
        this.selector = selector;
        this.optimize &= (unit.global.target == Global.TARGET_JVM);
        this.doBinding = doBinding;
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
            case ConstantPat(AConstant value):
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
            case AltPat(Header header):
                System.out.println(indent + "-- ALTERNATIVES:");
                print(header, indent + "   * ");
                print(patNode.and, indent + "   * -> ");
                if (patNode.or != null)
                    print(patNode.or, indent);
                break;
            case Body(_, Tree[] guards, Tree[] stats):
                if ((guards.length == 0) && (stats.length == 0))
                    System.out.println(indent + "true");
                else
                    System.out.println(indent + "BODY(" + stats.length + ")");
                break;
            }
    }

    /** enters a sequence of cases into the pattern matcher
     */
    public void construct(Tree[] cases) {
        for( int i = 0; i < cases.length; i++ )
            enter(cases[i]);
    }

    /** enter a single case into the pattern matcher
     */
    protected void enter(Tree caseDef) {
        switch (caseDef) {
        case CaseDef(Tree pat, Tree guard, Tree body):
            CaseEnv env = new CaseEnv(owner, unit);
            // PatternNode matched = match(pat, root);
            PatternNode target = enter1(pat, -1, root, root.symbol(), env);
            // if (target.and != null)
            //     unit.error(pat.pos, "duplicate case");
            if (target.and == null)
                target.and = mk.Body(caseDef.pos, env.boundVars(), guard, body);
            else if (target.and instanceof Body)
                updateBody((Body)target.and, env.boundVars(), guard, body);
            else
                unit.error(pat.pos, "duplicate case");
        }
    }

    protected void updateBody(Body tree, ValDef[] bound, Tree guard, Tree body) {
        if (tree.guard[tree.guard.length - 1] == Tree.Empty) {
            //unit.error(body.pos, "unreachable code");
        } else {
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
    if (next.subsumes( patNode ) &&
    ((target = match(patArgs, next)) != null))
    return target;
    else if (next.isDefaultPat())
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
        case Bind(_, Tree pat):
            return patternArgs(pat);
        case Apply(_, Tree[] args):
            if ( isSeqApply((Apply) tree)  && !delegateSequenceMatching)
                switch (args[0]) {
                case Sequence(Tree[] ts):
                    return ts;
                }
            return args;
        case Sequence(Tree[] ts):
            if (!delegateSequenceMatching)
                return ts;
            return Tree.EMPTY_ARRAY;
        default:
            return Tree.EMPTY_ARRAY;
        }
    }

    protected boolean isSeqApply( Tree.Apply tree ) {
        return (tree.args.length == 1  &&
                (tree.type.symbol().flags & Modifiers.CASE) == 0);
    }

    protected PatternNode patternNode(Tree tree, Header header, CaseEnv env) {
          //System.out.println("patternNode("+tree+","+header+")");
          switch (tree) {

        case Bind(Name name,
                  Typed( Ident( Names.PATTERN_WILDCARD ), Tree tpe)): // x@_:Type

              if(header.type.isSubType(tpe.type)) {
                    PatternNode node = mk.DefaultPat(tree.pos, tpe.type);
                    env.newBoundVar( tree.symbol(), tree.type, header.selector );
                    return node;
              } else {
                    ConstrPat node = mk.ConstrPat(tree.pos, tpe.type);
                    env.newBoundVar( tree.symbol(), tree.type, gen.Ident(tree.pos, node.casted));
                    return node;
              }

        case Bind( Name name, Ident( Names.PATTERN_WILDCARD )): // x @ _
            PatternNode node = mk.DefaultPat(tree.pos, header.type);
            if ((env != null) && (tree.symbol() != defs.PATTERN_WILDCARD))
                env.newBoundVar( tree.symbol(), tree.type, header.selector);
            return node;

        case Bind( Name name, Tree pat):
            PatternNode node = patternNode(pat, header, env);
            if ((env != null) && (tree.symbol() != defs.PATTERN_WILDCARD)) {
                Symbol casted = node.symbol();
                Tree theValue =  (casted == Symbol.NONE) ? header.selector : gen.Ident(tree.pos, casted);
                env.newBoundVar( tree.symbol(), tree.type, theValue );
            }
            return node;
        case Apply(Tree fn, Tree[] args):             // pattern with args
            if(isSeqApply((Apply)tree)) {
                if (!delegateSequenceMatching) {
                    switch (args[0]) {
                    case Sequence(Tree[] ts):
                        return mk.SequencePat( tree.pos, tree.type, ts.length );
                    }
                } else {
                    PatternNode res = mk.ConstrPat(tree.pos, tree.type);
                    res.and = mk.Header(tree.pos, header.type, header.selector);
                    res.and.and = mk.SeqContainerPat( tree.pos, tree.type, args[ 0 ] );
                    return res;
                }
            } else if ((fn.symbol() != null) && fn.symbol().isStable())
                return mk.VariablePat(tree.pos, tree);
            return mk.ConstrPat(tree.pos, tree.type);
        case Typed(Ident ident, Tree tpe):       // variable pattern
              boolean doTest = header.type.isSubType(tpe.type);
              PatternNode node = doTest ?
                    mk.DefaultPat(tree.pos, tpe.type)
                    : mk.ConstrPat(tree.pos, tpe.type);
              if ((env != null) && (ident.symbol() != defs.PATTERN_WILDCARD))
                    switch (node) {
                    case ConstrPat(Symbol casted):
                          env.newBoundVar(
                                          ((Tree.Typed)tree).expr.symbol(),
                                          tpe.type,
                                          gen.Ident(tree.pos, casted));
                          break;
                    default:
                          env.newBoundVar(
                                          ((Tree.Typed)tree).expr.symbol(),
                                          tpe.type,
                                          doTest ? header.selector : gen.Ident(tree.pos, ((ConstrPat) node).casted));
                }
            return node;
        case Ident(Name name):                  // pattern without args or variable
            if (tree.symbol() == defs.PATTERN_WILDCARD)
                return mk.DefaultPat(tree.pos, header.type);
            else if (tree.symbol().isPrimaryConstructor()) {
                assert false; // this may not happen ??  ----------------- Burak
                return mk.ConstrPat(tree.pos, tree.type);
            } else if (name.isVariable()) {// should be Bind  ------------ Burak
                assert false;
                if (env != null)
                    env.newBoundVar(tree.symbol(),
                                    tree.type,
                                    header.selector);
                return mk.DefaultPat(tree.pos, header.type);
            } else
                return mk.VariablePat(tree.pos, tree);
        case Select(_, Name name):                                    // variable
            if (tree.symbol().isPrimaryConstructor())
                return mk.ConstrPat(tree.pos, tree.type);
            else
                return mk.VariablePat(tree.pos, tree);
        case Literal(AConstant value):
            return mk.ConstantPat(tree.pos, tree.type, value);
        case Sequence(Tree[] ts):
            if ( !delegateSequenceMatching ) {
                return mk.SequencePat(tree.pos, tree.type, ts.length);
            } else {
                return mk.SeqContainerPat(tree.pos, tree.type, tree);
            }
        case Alternative(Tree[] ts):
            assert ts.length > 1;
            PatternNode subroot = mk.ConstrPat(header.pos, header.type);
            subroot.and = mk.Header(header.pos, header.type, header.selector.duplicate());
            CaseEnv subenv = new CaseEnv(owner, unit);
            for (int i = 0; i < ts.length; i++) {
                PatternNode target = enter1(ts[i], -1, subroot, subroot.symbol(), subenv);
                target.and = mk.Body(tree.pos);
            }
            return mk.AltPat(tree.pos, (Header)subroot.and);
        default:
            throw new ApplicationError("unit = " + unit + "; tree = "+tree);
        }
    }

    protected PatternNode enter(Tree pat,
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

    private Header newHeader( int pos, Symbol casted, int index ) {
        Tree ident = gen.Ident( pos, casted );
        if (casted.pos == Position.FIRSTPOS) {
            Tree t =
                gen.mkApply_V(
                              gen.Select( ident, defs.FUNCTION_APPLY( 1 )),
                              new Tree[]{ gen.mkIntLit( pos, index ) });
            Type seqType = t.type;
            return mk.Header( pos, seqType, t );
        } else {
            Symbol ts = ((ClassSymbol) casted.getType().symbol())
                .caseFieldAccessor(index);
            Type accType = casted.getType().memberType(ts);
            Tree accTree = gen.Select( ident, ts);
            switch (accType) {
                // scala case accessor
            case MethodType(_, _):
                return mk.Header(
                                 pos,
                                 accType.resultType(),
                                 gen.mkApply__(accTree));
                // jaco case accessor
            default:
                return mk.Header(pos, accType, accTree);
            }
        }
    }

    /** main enter function
     *
     *  invariant: ( curHeader == (Header)target.and ) holds
     */
    protected PatternNode enter1(Tree pat,
                              int index,
                              PatternNode target,
                              Symbol casted,
                              CaseEnv env) {
          //System.err.println("enter(" + pat + ", " + index + ", " + target + ", " + casted + ")");

        Tree[] patArgs = patternArgs(pat);        // get pattern arguments
        Header curHeader = (Header)target.and;    // advance one step in intermediate representation
        if (curHeader == null) {                  // check if we have to add a new header
            assert index >= 0 : casted;
            target.and = curHeader = newHeader( pat.pos, casted, index );
            curHeader.or = patternNode( pat, curHeader, env );
            return enter( patArgs, curHeader.or, casted, env );
        }

        // find most recent header
        while (curHeader.next != null)
            curHeader = curHeader.next;
        // create node
        PatternNode patNode = patternNode(pat, curHeader, env);
        PatternNode next = curHeader;

        // add branch to curHeader, but reuse tests if possible
        while (true)
            if ( next.isSameAs( patNode ) ) {           // test for patNode already present --> reuse
                  // substitute... !!!
                  switch( patNode ) {
                  case ConstrPat( Symbol ocasted ):
                        env.substitute( ocasted, gen.Ident(patNode.pos,
                                                           ((ConstrPat) next).casted));
                  }
                  return enter(patArgs, next, casted, env);
            }
            else if ( next.isDefaultPat() ||         // default case reached, or
                      ((next.or == null) &&          //  no more alternatives and
                       ( patNode.isDefaultPat() || next.subsumes( patNode )))) {
                  // new node is default or subsumed

                return enter(                        // create independent new header , because cannot use this one
                             patArgs,
                             (curHeader = (curHeader.next =
                                           mk.Header(patNode.pos, curHeader.type, curHeader.selector))).or
                             = patNode,
                             casted,
                             env);
            }
            else if (next.or == null) {
                return enter(patArgs, next.or = patNode, casted, env); // add new branch
            }
            else
                next = next.or;
    }

    /** calls enter for an array of patterns, see enter
     */
    protected PatternNode enter(Tree[] pats, PatternNode target, Symbol casted, CaseEnv env) {
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
            Type tpe = tree.type.symbol().primaryConstructor().type();
            //System.out.println("~~~ " + tree.type() + ", " + tree.type().symbol().primaryConstructor());
            switch (tpe) {
                // I'm not sure if this is a good idea, but obviously, currently all case classes
                // without constructor arguments have type NoType
            case NoType:
                assert false;
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
        if (optimize && isSimpleIntSwitch())
            return intSwitchToTree();
        else if (false && optimize && isSimpleSwitch())
            return switchToTree();
        else
            return generalSwitchToTree();
    }

    protected boolean isSimpleIntSwitch() {
        if (selector.type.widen().isSameAs(defs.INT_TYPE())) {
            PatternNode patNode = root.and;
            while (patNode != null) {
                PatternNode node = patNode;
                while ((node = node.or) != null) {
                    switch (node) {
                    case ConstantPat(_):
                        break;
                    default:
                        return false;
                    }
                    switch (node.and) {
                    case Body(ValDef[][] bound, Tree[] guard, _):
                        if ((guard.length > 1) ||
                            (guard[0] != Tree.Empty) ||
                            (bound[0].length > 0))
                            return false;
                        break;
                    default:
                        return false;
                    }
                }
                patNode = patNode.next();
            }
            return true;
        } else
            return false;
    }

    protected boolean isSimpleSwitch() {
        print();
        PatternNode patNode = root.and;
        while (patNode != null) {
            PatternNode node = patNode;
            while ((node = node.or) != null) {
                boolean isCase = false;
                switch (node) {
                case VariablePat(Tree tree):
                    System.out.println(((tree.symbol().flags & Modifiers.CASE) != 0));
                    break;
                case ConstrPat(_):
                    System.out.println(node.type + " / " + ((node.type.symbol().flags & Modifiers.CASE) != 0));
                    PatternNode inner = node.and;
                    outer: while (true) {
                        switch (inner) {
                        case Header(_, Header next):
                            if (next != null)
                                return false;
                            inner = inner.or;
                            break;
                        case DefaultPat():
                            inner = inner.and;
                            break;
                        case Body(ValDef[][] bound, Tree[] guard, _):
                            if ((guard.length > 1) ||
                                (guard[0] != Tree.Empty))
                                return false;
                            break outer;
                        default:
                            System.out.println(inner);
                            return false;
                        }
                    }
                    break;
                default:
                    return false;
                }
            }
            patNode = patNode.next();
        }
        return true;
    }

    static class TagBodyPair {
        int tag;
        Tree body;
        TagBodyPair next;

        TagBodyPair(int tag, Tree body, TagBodyPair next) {
            this.tag = tag;
            this.body = body;
            this.next = next;
        }

        int length() {
            return (next == null) ? 1 : (next.length() + 1);
        }
    }

    static TagBodyPair insert(int tag, Tree body, TagBodyPair current) {
        if (current == null)
            return new TagBodyPair(tag, body, null);
        else if (tag > current.tag)
            return new TagBodyPair(current.tag, current.body, insert(tag, body, current.next));
        else
            return new TagBodyPair(tag, body, current);
    }

    protected int numCases(PatternNode patNode) {
        int n = 0;
        while ((patNode = patNode.or) != null)
            switch (patNode) {
            case DefaultPat():
                break;
            default:
                n++;
            }
        return n;
    }

    protected Tree defaultBody(PatternNode patNode, Tree otherwise) {
        while (patNode != null) {
            PatternNode node = patNode;
            while ((node = node.or) != null)
                switch (node) {
                case DefaultPat():
                    return bodyToTree(node.and);
                }
            patNode = patNode.next();
        }
        return otherwise;
    }

    /** This method translates pattern matching expressions that match
     *  on integers on the top level.
     */
    public Tree intSwitchToTree() {
        //print();
        int ncases = numCases(root.and);
        Tree matchError = cf.ThrowMatchError(selector.pos, resultVar.getType());
        // without a case, we return a match error if there is no default case
        if (ncases == 0)
            return defaultBody(root.and, matchError);
        // for one case we use a normal if-then-else instruction
        else if (ncases == 1) {
            switch (root.and.or) {
            case ConstantPat(AConstant value):
                return gen.If(
                              cf.Equals(selector,
                                        gen.Literal(root.and.or.pos, value)),
                              bodyToTree(root.and.or.and),
                              defaultBody(root.and, matchError));
            default:
                return generalSwitchToTree();
            }
        }
        // if we have more than 2 cases than use a switch statement
        switch (root.and) {
        case Header(_, Header next):
            TagBodyPair mappings = null;
            Tree defaultBody = null;
            PatternNode patNode = root.and;
            while (patNode != null) {
                PatternNode node = patNode.or;
                while (node != null) {
                    switch (node) {
                    case DefaultPat():
                        if (defaultBody != null)
                            throw new ApplicationError();
                        defaultBody = bodyToTree(node.and);
                        node = node.or;
                        break;
                    case ConstantPat(INT(int value)):
                        mappings = insert(
                                          value,
                                          bodyToTree(node.and),
                                          mappings);
                        node = node.or;
                        break;
                    default:
                        throw new ApplicationError(node.toString());
                    }
                }
                patNode = patNode.next();
            }
            if (defaultBody == null)
                defaultBody = cf.ThrowMatchError(selector.pos, resultVar.getType());
            if (mappings == null) {
                return gen.Switch(selector, new int[0], new Tree[0], defaultBody, resultVar.getType());
            } else {
                int n = mappings.length();
                int[] tags = new int[n];
                Tree[] bodies = new Tree[n];
                n = 0;
                while (mappings != null) {
                    tags[n] = mappings.tag;
                    bodies[n++] = mappings.body;
                    mappings = mappings.next;
                }
                return gen.Switch(selector, tags, bodies, defaultBody, resultVar.getType());
            }
        default:
            throw new ApplicationError();
        }
    }

    protected Tree bodyToTree(PatternNode node) {
        switch (node) {
        case Body(_, _, Tree[] body):
            return body[0];
        default:
            throw new ApplicationError();
        }
    }

    public Tree switchToTree() {
        throw new Error();
    }

    public Tree generalSwitchToTree() {
        Tree[] ts = {
            gen.ValDef(root.symbol(), selector),
            gen.ValDef(resultVar, gen.mkDefaultValue(selector.pos, resultVar.getType()))};
        Tree res = gen.If(
                          selector.pos,
                          toTree(root.and),
                          gen.Ident(selector.pos, resultVar),
                          cf.ThrowMatchError(selector.pos, resultVar.getType()));
        return gen.mkBlock(selector.pos, ts, res);
    }

    protected Tree toTree(PatternNode node) {
        Tree res = gen.mkBooleanLit(node.pos, false);
        while (node != null)
            switch (node) {
            case Header(Tree selector, Header next):
                //res = cf.And(mkNegate(res), toTree(node.or, selector));
                //System.out.println("HEADER TYPE = " + selector.type);
                if (optimize(node.type, node.or))
                    res = cf.Or(res, toOptTree(node.or, selector));
                else
                    res = cf.Or(res, toTree(node.or, selector));
                node = next;
                break;
            case Body(ValDef[][] bound, Tree[] guard, Tree[] body):
                if ((bound.length == 0) &&
                    (guard.length == 0) &&
                    (body.length == 0)) {
                    return gen.mkBooleanLit(node.pos, true); // cf.Or(res, gen.mkBooleanLit(node.pos, true));
                } else if (!doBinding)
                    bound = new ValDef[][]{new ValDef[]{}};
                for (int i = guard.length - 1; i >= 0; i--) {
                    Tree[] ts = bound[i];
                    Tree res0 = gen.mkBlock(
                                            gen.Assign(
                                                       gen.Ident(body[i].pos, resultVar),
                                                       body[i]),
                                            gen.mkBooleanLit(body[i].pos, true));
                    if (guard[i] != Tree.Empty)
                        res0 = cf.And(guard[i], res0);
                    res = cf.Or(gen.mkBlock(body[i].pos, ts, res0), res);
                }
                return res;
            default:
                throw new ApplicationError();
            }
        return res;
    }

    protected boolean optimize(Type selType, PatternNode alternatives) {
        if (!optimize || !selType.isSubType(defs.OBJECT_TYPE()))
            return false;
        int cases = 0;
        while (alternatives != null) {
            switch (alternatives) {
            case ConstrPat(_):
                if (alternatives.type.symbol().isCaseClass())
                    cases++;
                else
                    return false;
                break;
            case DefaultPat():
                break;
            default:
                return false;
            }
            alternatives = alternatives.or;
        }
        return cases > 2;
    }

    static class TagNodePair {
        int tag;
        PatternNode node;
        TagNodePair next;

        TagNodePair(int tag, PatternNode node, TagNodePair next) {
            this.tag = tag;
            this.node = node;
            this.next = next;
        }

        int length() {
            return (next == null) ? 1 : (next.length() + 1);
        }
    }

    static TagNodePair insert(int tag, PatternNode node, TagNodePair current) {
        if (current == null)
            return new TagNodePair(tag, node, null);
        else if (tag > current.tag)
            return new TagNodePair(current.tag, current.node, insert(tag, node, current.next));
        else if (tag == current.tag) {
            PatternNode old = current.node;
            (current.node = node).or = old;
            return current;
        } else
            return new TagNodePair(tag, node, current);
    }

    static TagNodePair insertNode(int tag, PatternNode node, TagNodePair current) {
        PatternNode newnode = node.dup();
        newnode.or = null;
        return insert(tag, newnode, current);
    }

    protected Tree toOptTree(PatternNode node, Tree selector) {
        //System.err.println("pm.toOptTree called"+node);
        TagNodePair cases = null;
        PatternNode defaultCase = null;
        while (node != null)
            switch (node) {
            case ConstrPat(Symbol casted):
                cases = insertNode(node.type.symbol().tag(), node, cases);
                node = node.or;
                break;
            case DefaultPat():
                defaultCase = node;
                node = node.or;
                break;
            default:
                throw new ApplicationError();
            }
        int n = cases.length();
        int[] tags = new int[n];
        Tree[] bodies = new Tree[n];
        n = 0;
        while (cases != null) {
            tags[n] = cases.tag;
            bodies[n++] = toTree(cases.node, selector);
            cases = cases.next;
        }
        return gen.Switch(
                          gen.mkApply__(gen.Select(selector.duplicate(), defs.OBJECT_TAG())),
                          tags,
                          bodies,
                          (defaultCase == null) ? gen.mkBooleanLit(selector.pos, false)
                          : toTree(defaultCase.and),
                          defs.BOOLEAN_TYPE());
    }

    protected Tree toTree(PatternNode node, Tree selector) {
          //System.err.println("pm.toTree("+node+","+selector+")");
        if (node == null)
            return gen.mkBooleanLit(selector.pos, false);
        switch (node) {
        case DefaultPat():
            return toTree(node.and);
        case ConstrPat(Symbol casted):
            return gen.If(
                          gen.mkIsInstanceOf(selector.duplicate(), node.type),
                          gen.mkBlock(
                                      gen.ValDef(casted,
                                                 gen.mkAsInstanceOf(selector.duplicate(), node.type)),
                                      toTree(node.and)),
                          toTree(node.or, selector.duplicate()));
        case SequencePat(Symbol casted, int len):
            return gen.If(
                          cf.And(
                                 gen.mkIsInstanceOf(selector.duplicate(), node.type),
                                 cf.Equals(
                                           gen.mkApply__(
                                                         gen.Select(
                                                                    gen.mkAsInstanceOf(
                                                                                       selector.duplicate(), node.type),
                                                                    defs.SEQ_LENGTH())),
                                           gen.mkIntLit(selector.pos, len))),
                          gen.mkBlock(
                                      gen.ValDef(casted,
                                                 gen.mkAsInstanceOf(selector.duplicate(), node.type)),
                                      toTree(node.and)),
                          toTree(node.or, selector.duplicate()));
        case ConstantPat(AConstant value):
            return gen.If(
                          cf.Equals(selector.duplicate(),
                                    gen.Literal(selector.pos, value)),
                          toTree(node.and),
                          toTree(node.or, selector.duplicate()));
        case VariablePat(Tree tree):
            return gen.If(
                          cf.Equals(selector.duplicate(), tree),
                          toTree(node.and),
                          toTree(node.or, selector.duplicate()));
        case AltPat(Header header):
            return gen.If(
                          toTree(header),
                          toTree(node.and),
                          toTree(node.or, selector.duplicate()));
        default:
            throw new ApplicationError();
        }
    }
}

/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import scalac.*;
import scalac.ast.*;
import scalac.symtab.*;
import scalac.typechecker.*;

import PatternNode.*;
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;
import scalac.util.Name ;
import scalac.util.Names ;
//import scalac.ast.printer.TextTreePrinter ;

import java.util.Vector ;
import java.util.Iterator ;

/** Matthias' algebraic pattern matcher, with some things factored out
 *  @author Matthias Zenger, Burak Emir
 */

public class AlgebraicMatcher extends PatternTool {

      //public static final Name RESULT_N = Name.fromString("$result");
      public static final String RESULT_N = "amResult";

      Matcher _m;

      // the following instance variables are inherited from AbstractMatcher
      // unit, fresh, make, defs, gen, infer, statics, trueSym, falseSym;

      /** the root of the pattern node structure
       */
      PatternNode root;

      /** the symbol of the result variable
       */
      Symbol resultVar;

      PatternNodeCreator mk;
      CodeFactory        cf;

      boolean doBinding;

      /** constructor
       */
      public AlgebraicMatcher( Unit unit, Infer infer ) {
            super( unit, infer );
      }

      /** constructs an algebraic pattern matcher from cases
       */
      public void construct( Matcher m, Tree[] cases) {
            construct(m, cases, true);
      }

      /** constructs an algebraic pattern matcher from cases
       */
      public void construct( Matcher m, Tree[] cases, boolean doBinding) {
            this.doBinding = doBinding;
            this._m = m;
            initialize();
            for( int i = 0; i < cases.length; i++ ) {
                  addCase( (CaseDef) cases[i], i);
            }
            _m.tree = toTree();
      }


      /** initializes this AlgebraicMatcher, see Matcher.initialize
       */
      void initialize() {

            this.mk = new PatternNodeCreator( unit, infer, _m.owner/*_m.clazzOwner*/ );
            this.cf = new CodeFactory( unit, infer /*, _m.pos*/ );

            this.root = mk.ConstrPat(_m.pos,
                                      _m.selector.type);
            this.root.and = mk.Header(_m.pos,
                                       _m.selector.type,
                                      gen.Ident( _m.pos, root.symbol() )
                                      .setType(_m.selector.type));

            this.resultVar = new TermSymbol(_m.pos,
                                            cf.fresh.newName(RESULT_N),
                                            _m.owner,
                                            0);
            //this.resultType = resultType;
            this.resultVar.setType( _m.resultType );

            //System.out.println(" constructed matcherDefs ");
      }

    /** return the analyzed type
     */
    public Type typeOf(Symbol sym) {
        return sym.type();
    }

    /** return the analyzed type
     */
    public Type typeOf0(Symbol sym) {
        return sym.typeAt(unit.global.PHASE.ANALYZER.id());
    }

    /** factories
     */

    public void updateBody(Body tree, ValDef[] bound, Tree guard, Tree body) {
        if (tree.guard[tree.guard.length - 1] == Tree.Empty)
            unit.error(body.pos, "unreachable code");
        // BINDING BELOW SEQUENCE
        //System.out.println("AlgebraicMatcher::updateBody ... , freeVars"+_m.freeVars);
        /*
        for( int i=0; i < bound.length; i++ )
              _m.freeVars.add( bound[ i ].symbol() );
        */
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

      /*
    public Tree copy(Tree tree) {
        return tree; // insert copy function here
    }
      */

      /** convenience method, see addCase below
       */
    public void addCase(CaseDef tree, int case_index) {
          //System.out.println( "addCase(" +
          //                    TextTreePrinter.toString(tree.pat) +
          //                    ")" );
        addCase(tree.pos, tree.pat, tree.guard, tree.body, case_index);
        //root.and.prettyPrint();
    }
      /** adds a case definition to the intermediate presentation
       */
    protected CaseEnv addCase(int pos, Tree pat, Tree guard, Tree body, int case_index) {

        CaseEnv env = new CaseEnv( _m.owner, unit );

        //PatternNode matched = match(pat, root);
        PatternNode target = enter1(pat, /*null*/ -1, root, root.symbol(), env, case_index);
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


    protected Tree[] patternArgs(Tree tree) {
	//System.err.println("patternArgs("+tree+")");
        switch (tree) {
            case Apply(_, Tree[] args):
		if( isSeqApply( (Apply) tree )) {
		    //System.err.println("patternArgs: is seq apply !");
		    return Tree.EMPTY_ARRAY;// let sequence matcher handle this

		}
                return args;
            default:
                return Tree.EMPTY_ARRAY;
        }
    }

    protected Type getConstrType(Type tpe) {
	return tpe;
    }

    protected Type getHeaderType(Type tpe) {
        switch (tpe) {
            case PolyType(_, Type res):
                return res;
            default:
                return tpe;
        }
    }

      /** constructs a pattern node depending on the case of argument `tree'
       *  @param tree       the pattern node that needs to be translated
       *  @param castType cast type of the position that is tested
       *  @param selector   selector tree of the position that is tested
       *  @param env    an environment
       */

      protected PatternNode patternNode(Tree tree,
                                      Type castType,
                                      Tree selector,
                                      CaseEnv env,
                                      int case_index) {
          Type theType = getConstrType( tree.type );
          switch (tree) {
	  case Apply(Tree fn, Tree[] args):             // pattern with args
	      if( isSeqApply( (Apply) tree ))
		  return mk.SeqContainerPat( tree.pos, theType, args[ 0 ] );
	      return mk.ConstrPat(tree.pos, theType);
          case Typed(Ident(Name name), Tree tpe):       // typed pattern
                theType = getConstrType( tpe.type );
                assert (env != null ) : "env is null";
                if (/*(env != null) &&*/ (name != Names.WILDCARD))
                      env.newBoundVar(tree.pos,
                                      ((Tree.Typed)tree).expr.symbol(),
                                      theType,
                                      selector);
                if (castType.isSubType( theType ))
                      // castType is a subtype of theType
                      return mk.DefaultPat( tree.pos, theType );
                else
                      return mk.ConstrPat( tree.pos, theType );
          case Bind(Name name, Ident(Name id)): // x @ _
                if( id == Names.WILDCARD ) {
                      env.newBoundVar(tree.pos,
                                      tree.symbol(),
                                      theType,
                                      selector);
                      return mk.DefaultPat(tree.pos, getConstrType(castType));
                }
                throw new ApplicationError("cannot handle "+tree);
          case Ident(Name name):        // pattern without args or variable
                if ((tree.symbol() != null) && tree.symbol().isPrimaryConstructor())
                      return mk.ConstrPat(tree.pos, theType);
                else if (name.isVariable()) {
                      assert (env != null ) : "env is null";
                      if (/*(env != null) &&*/ (name != Names.WILDCARD))
                            env.newBoundVar(tree.pos,
                                            tree.symbol(),
                                            theType,
                                            selector);
                      return mk.DefaultPat(tree.pos, getConstrType(castType));
                } else
                      return mk.VariablePat(tree.pos, tree);
          case Select(_, Name name):                                    // variable
                if (tree.symbol().isPrimaryConstructor())
                      return mk.ConstrPat(tree.pos, theType);
                else
                    return mk.VariablePat(tree.pos, tree);
          case Literal(Object value):
	      return mk.ConstantPat(tree.pos, theType, value);

	  case Sequence( _ ):
	      throw new ApplicationError("Illegal pattern");
	  default:
	      throw new ApplicationError("cannot handle "+tree);
	  }
      }

      /** returns true if p and q are pattern nodes of the same kind and p matches
       *  whenever q matches, possibly even more often
       */
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
           case ConstantPat( Object pval ):
                switch (q) {
                    case ConstantPat( Object qval ):
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

      /** convenience method, if target there is a ConstrPat,
       *  the use its sym instead of the argument given.
       */
      /*
    public PatternNode eneter(Tree pat,
                             Symbol typeSym, //int index,
                               PatternNode target,
                               Symbol casted,
                               CaseEnv env,
                             int case_index) {
        switch (target) {
        case ConstrPat(Symbol newCasted):
              return enter1(pat, typeSym, target, newCasted, env, case_index);
        default:
              return enter1(pat, typeSym, target, casted, env, case_index);
        }
    }
*/
      /** the main enter function.
       *  @param typeSym
       *  @param target we hang the result to the .and successor of this node,
       *                possibly creating a new header.
       */
    public PatternNode enter1(Tree pat,
                              int index,
                              //Symbol typeSym,
                              PatternNode target,
                              Symbol casted,
                              CaseEnv env,
                              int case_index) {
	//System.out.println("enter(" + pat + ", " + /*typeSym*/ index + ", " + target + ", " + casted + ")");
        // get pattern arguments (if applicable)
        Tree[] patArgs = patternArgs(pat);
        // get case fields
        //assert patArgs.length == nCaseComponents(pat); // commented out also in new matcher
        // advance one step in pattern
        Header curHeader = (Header)target.and;
        // check if we have to add a new header
        if (curHeader == null) {
              //assert index >= 0 : casted;
              // <casted>.as[ <caseFieldAccessor[ index ]> ]

              //assert typeSym != null;

              Symbol typeSym = ((ClassSymbol) casted.type().symbol())
                    .caseFieldAccessor(index);

              Type castType = getHeaderType( typeOf0( typeSym )).asSeenFrom(typeOf(casted), typeSym.owner());
              target.and = curHeader =
                    mk.Header(pat.pos,
                              castType,
                              make.Apply(pat.pos,
                                         make.Select(pat.pos,
                                                     gen.Ident(pat.pos, casted),
                                                     typeSym.name)
                                         .setType(Type.MethodType(Symbol.EMPTY_ARRAY,
                                                               castType))
                                         .setSymbol( typeSym ),
                                         Tree.EMPTY_ARRAY).setType( castType ));
              // translate the root of `pat'
              curHeader.or = patternNode(pat,
                                         curHeader.type,
                                         curHeader.selector,
                                         env,
                                         case_index);
              // translate the args of `pat'
              return enter(patArgs, curHeader.or, casted, env, case_index);
        }
        // find most recent header - (the last -next- successor of curHeader)
        while (curHeader.next != null)
            curHeader = curHeader.next;
         // translate the root of `pat'
        PatternNode patNode = patternNode(pat,
                                          curHeader.type,
                                          curHeader.selector,
                                          env,
                                          case_index);
        PatternNode next = curHeader;
        // enter node
        while (true)
            if (superPat(next, patNode))
                  // next is of the same kind as patNode and matches (supertype)
                return enter(patArgs, next, casted, env, case_index);
            else if (isDefaultPat(next) ||
                     ((next.or == null) && isDefaultPat(patNode))) {
                  curHeader.next = mk.Header(patNode.pos,
                                              curHeader.type, curHeader.selector);
                  curHeader.next.or = patNode;
                  return enter(patArgs,
                               patNode,
                               casted,
                               env,
                               case_index);
                  }
            else if (next.or == null) {
		next.or = patNode;

		if (patArgs.length == 1  && (casted.flags & Modifiers.CASE) == 0) {
		    return enter( patArgs, patNode, casted, env, case_index );	 // FIX
		}

		return enter( patArgs, patNode, casted, env, case_index );
            } else
                next = next.or;
    }

    boolean isSeqApply( Tree.Apply tree ) {
	return (tree.args.length == 1  && (tree.type.symbol().flags & Modifiers.CASE) == 0);
    }

      /** calls enter for an array of patterns, see enter
       */
    public PatternNode enter(Tree[] pats, PatternNode target, Symbol casted, CaseEnv env, int case_index) {
        switch (target) {
            case ConstrPat(Symbol newCasted):
                casted = newCasted;
		break;
            case SeqContainerPat(Symbol newCasted, _):
		casted = newCasted;
		//target = enter(pats, target.or, casted, env, case_index);
        }
        for (int i = 0; i < pats.length; i++) {
	    //Symbol typeSym = ((ClassSymbol) casted.type().symbol())
	    //      .caseFieldAccessor( i );
	    target = enter1(pats[i], /*typeSym*/ i, target, casted, env, case_index);
        }
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
        ts.append( gen.ValDef(_m.pos, root.symbol(), _m.selector ));
        ts.append( gen.ValDef(_m.pos, resultVar, Tree.Empty ));
        ts.append( cf.If( toTree(root.and),
                          gen.Ident( _m.pos, resultVar ),
                          cf.ThrowMatchError( _m.pos, _m.resultType )));
        /*
            make.If(
                _m.pos,
                toTree(root.and),
                gen.Ident( _m.pos, resultVar ),
                cf.ThrowMatchError( _m.resultType )).type( _m.resultType ));
        */
        return cf.Block(_m.pos, ts.toArray(), _m.resultType);
    }

    protected Tree toTree(PatternNode node) {
        Tree res = gen.mkBooleanLit( node.pos, false );
        while (node != null)
            switch (node) {
                case Header(Tree selector, Header next):
                    res = cf.Or(res, toTree(node.or, selector));
                    node = next;
                    break;
                case Body(ValDef[][] bound, Tree[] guard, Tree[] body):
                    for (int i = guard.length - 1; i >= 0; i--) {
                          Tree[] ts;
                          if( doBinding ) {
                                ts = new Tree[bound[i].length + 1];
                                System.arraycopy(bound[i], 0, ts, 0, bound[i].length);
                          } else
                                ts = new Tree[ 1 ];

                        // FOR BIN
                        /*
                        System.out.print("AM.toTree(Body(...)) ValDefs: ");
                        for( int j = 0; j < bound[i].length; j++ )
                              System.out.print(" "+bound[i][j].symbol());
                        System.out.println();
                        */
                        //System.out.print("freeVars: "+_m.freeVars);

                          int last = ts.length - 1;
                        ts[ last ] = cf.Block(body[i].pos,
                                              new Tree[]{
                                                    gen.Assign(gen.Ident( body[i].pos, resultVar ),
                                                               body[i]),
                                                    gen.mkBooleanLit(body[i].pos, true)
                                              }, defs.BOOLEAN_TYPE);
                        if (guard[i] != Tree.Empty)
                              ts[ last ] = cf.And(guard[i], ts[ last ]);
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
            return gen.mkBooleanLit(_m.pos, false);
        switch (node) {
            case DefaultPat():
                return toTree(node.and);
            case ConstrPat(Symbol casted):
                return make.If(
                        _m.pos,
                        cf.Is(selector, node.type),
                        cf.Block(selector.pos,
                            new Tree[]{
                                make.ValDef(selector.pos,
                                            //Kinds.VAL,
                                            0,
                                            casted.name,
                                            gen.mkType(selector.pos, node.type),
                                            cf.As(selector, node.type))
                                    .setType(defs.UNIT_TYPE).setSymbol(casted),
                                toTree(node.and)}, defs.BOOLEAN_TYPE),
                        toTree(node.or, selector)).setType(defs.BOOLEAN_TYPE);
            case ConstantPat( Object value ):
                return make.If(
                        selector.pos,
                        cf.Equals(selector,
				  make.Literal(selector.pos, /*kind, */value)
                                .setType(node.type)),
                        toTree(node.and),
                        toTree(node.or, selector)).setType(defs.BOOLEAN_TYPE);
        case VariablePat(Tree tree):
	    //System.out.print("VariablePat");
	    //System.out.println( tree );
              return make.If( selector.pos,
                              cf.Equals(selector, tree),
                              toTree(node.and),
                              toTree(node.or, selector)).setType(defs.BOOLEAN_TYPE);
        case SeqContainerPat( _, _ ):
              return  callSequenceMatcher( node,
                                           selector );
        default:
              throw new ApplicationError();
        }
    }

      /** collects all sequence patterns and returns the default
       */
      PatternNode collectSeqPats( PatternNode node,
                                  Vector seqPatNodes,
                                  Vector bodies ) {

              PatternNode defaultNode = null;

              do {
                    if( node == null )
                          break;// defaultNode = node;
                    else
                          switch( node ) {
                    case SeqContainerPat( _, _ ):
                          seqPatNodes.add( node );
                          bodies.add( toTree( node.and ) );
                          node = node.or;
                          break;
                    default:
                          defaultNode = node;
                    }
              } while (defaultNode == null) ;

              return defaultNode;
      }

      Tree callSequenceMatcher( PatternNode node,
                                Tree selector) {

              /*    ???????????????????????? necessary to test whether is a Seq?
               make.If(selector.pos,
                             maybe cf.And( cf.Is(selector, seqpat.type())
                             ...
              */

              // translate the _.and subtree of this SeqContainerPat

              Vector seqPatNodes = new Vector();
              Vector bodies      = new Vector();

              PatternNode defaultNode = collectSeqPats( node,
                                                        seqPatNodes,
                                                        bodies );

              Tree defaultCase = toTree( defaultNode, selector );

              SequenceMatcher wordRec = new SequenceMatcher(unit, infer);

              Matcher m = new Matcher( _m.owner,
                                       selector,
                                       defs.BOOLEAN_TYPE );

              Tree pats[] = new Tree[ seqPatNodes.size() ];
              Tree body[] = new Tree[ seqPatNodes.size() ];

              Object tmp[] = bodies.toArray();
              int j = 0;
              for( Iterator it = seqPatNodes.iterator();
                   it.hasNext();) {
                    pats[ j ]  = ((SeqContainerPat) it.next()).seqpat;
                    body[ j ] = (Tree) tmp[j];
                          j++;
              }
              //Tree defaultTree = toTree( node.or, selector ); // cdef.body ;

              wordRec.construct( m, pats, body, defaultCase, doBinding );

              //_m.defs.addAll( m.defs  );

              return m.tree;

              //case BoundPat( Symbol var, PatternNode pat ):

      }

      /* // unused, but helps understanding
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
                    next = header.or;
                    while (next != null) {
                        if (superPat(next, patNode) &&
                            ((target = match(patArgs, next)) != null))
                            return target;
                        else if (isDefaultPat(next))
                            return next.and;
                    next = next.or;
                    }
                    header = header.next;
                } while (header != null);
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


}

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

public class AlgebraicMatcher extends PatternMatcher {

      Matcher _m;

      /** constructor
       */
      public AlgebraicMatcher( Unit unit ) {
            super( unit );
	    this.delegateSequenceMatching = true;
	    this.optimize = false;
      }

      /** constructs an algebraic pattern matcher from cases
       */
      public void construct( Matcher m, Tree[] cases) {
            construct(m, cases, true);
      }

      /** constructs an algebraic pattern matcher from cases
       */
      public void construct( Matcher m, Tree[] cases, boolean doBinding) {
            this._m = m;
	    super.initialize( _m.selector, _m.owner, _m.resultType, doBinding );
            for( int i = 0; i < cases.length; i++ ) {
                  addCase( (CaseDef) cases[i], i);
            }
            _m.tree = toTree();
      }


      /** initializes this AlgebraicMatcher, see Matcher.initialize
      void initialize() {
      }
       */

    /** return the analyzed type
     */
    public Type typeOf(Symbol sym) {
        return sym.type();
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
	//System.err.println("root"+root);
        //PatternNode matched = match(pat, root);
        PatternNode target = enter1(pat, //null
				    -1, root, root.symbol(), env
				    );
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
		/*
		if( isSeqApply( (Apply) tree )) {
		    //System.err.println("patternArgs: is seq apply !");
		    return Tree.EMPTY_ARRAY;// let sequence matcher handle this

		}
		if( isStarApply( (Apply) tree )) {
		    return Tree.EMPTY_ARRAY;// let sequence matcher handle this

		}
		*/
                return args;
            default:
                return Tree.EMPTY_ARRAY;
        }
    }

    protected Type getConstrType(Type tpe) {
	return tpe;
    }

    protected Type getHeaderType(Symbol sym) {
        return sym.type().resultType();
    }

      /** constructs a pattern node depending on the case of argument `tree'
       *  @param tree       the pattern node that needs to be translated
       *  @param castType cast type of the position that is tested
       *  @param selector   selector tree of the position that is tested
       *  @param env    an environment
       */

      protected PatternNode patternNode__(Tree tree,
                                      Type castType,
                                      Tree selector,
                                      CaseEnv env) {
	  //System.err.println("patternNode:"+tree );
          Type theType = getConstrType( tree.type );
          switch (tree) {
	  case Apply(Tree fn, Tree[] args):             // pattern with args
	      //System.err.println("Apply, isSeq?" + isSeqApply( (Apply) tree ));
	      //System.err.println("Apply, isStar?" + isStarApply( (Apply) tree ));
	      if( isSeqApply( (Apply) tree )) {
		  PatternNode res = mk.ConstrPat(tree.pos, theType);
		  res.and = mk.Header(tree.pos, castType, selector);
		  res.and.and = mk.SeqContainerPat( tree.pos, theType, args[ 0 ] );
	      	  return res;
	      }
	      return mk.ConstrPat(tree.pos, theType);
          case Typed(Ident ident, Tree tpe):       // typed pattern
                theType = getConstrType( tpe.type );
                assert (env != null ) : "env is null";
                if (/*(env != null) &&*/ (ident.symbol() != defs.PATTERN_WILDCARD))
                      env.newBoundVar(
                                      ((Tree.Typed)tree).expr.symbol(),
                                      theType,
                                      selector);
                if (castType.isSubType( theType ))
                      // castType is a subtype of theType
                      return mk.DefaultPat( tree.pos, theType );
                else
                      return mk.ConstrPat( tree.pos, theType );
          case Bind(Name name, Ident ident): // x @ _
                if( ident.symbol() == defs.PATTERN_WILDCARD ) {
                      env.newBoundVar(
                                      tree.symbol(),
                                      theType,
                                      selector);
                      return mk.DefaultPat(tree.pos, getConstrType(castType));
                }
                throw new ApplicationError("cannot handle "+tree);
          case Ident(Name name):        // pattern without args or variable
              Symbol symbol = tree.symbol();
              assert symbol != null: tree;
                if (symbol.isPrimaryConstructor())
                      return mk.ConstrPat(tree.pos, theType);
                else if (name.isVariable()) {
                      assert (env != null ) : "env is null";
                      if (/*(env != null) &&*/ (symbol != defs.PATTERN_WILDCARD))
                            env.newBoundVar(
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
	      return mk.SeqContainerPat( tree.pos, theType, tree );
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

      /** the main enter function.
       *  @param target we hang the result to the .and successor of this node,
       *                possibly creating a new header.
       */


    public PatternNode enter1__(Tree pat,
                              int index,
                              PatternNode target,
                              Symbol casted,
                              CaseEnv env ) {
	//System.out.println("enter("+pat+", "+index+", "+target+", "+casted+")");
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

              Type castType = getHeaderType(typeSym).asSeenFrom(typeOf(casted), typeSym.owner());
              target.and = curHeader =
                    mk.Header(pat.pos,
                              castType,
                              gen.mkApply__(gen.Select(gen.Ident(pat.pos, casted), typeSym)));
              // translate the root of `pat'
              curHeader.or = patternNode(pat,
                                         curHeader,/*.type,
						    curHeader.selector, */
                                         env);
              // translate the args of `pat'
              return enter(patArgs, curHeader.or, casted, env);
        }
        // find most recent header - (the last -next- successor of curHeader)
        while (curHeader.next != null)
            curHeader = curHeader.next;
         // translate the root of `pat'
        PatternNode patNode = patternNode(pat,
                                          curHeader,/*.type,
						     curHeader.selector, */
                                          env);
        PatternNode next = curHeader;
        // enter node
        while (true)
            if (superPat(next, patNode))
                  // next is of the same kind as patNode and matches (supertype)
                return enter(patArgs, next, casted, env);
            else if (isDefaultPat(next) ||
                     ((next.or == null) && isDefaultPat(patNode))) {
                  curHeader.next = mk.Header(patNode.pos,
                                              curHeader.type, curHeader.selector);
                  curHeader.next.or = patNode;
                  return enter(patArgs,
                               patNode,
                               casted,
                               env);
                  }
            else if (next.or == null) {
		next.or = patNode;

		if (patArgs.length == 1  && (casted.flags & Modifiers.CASE) == 0) {
		    return enter( patArgs, patNode, casted, env );	 // FIX
		}

		return enter( patArgs, patNode, casted, env );
            } else
                next = next.or;
    }


    /*
    boolean isSeqApply( Tree.Apply tree ) {
	return (tree.args.length == 1  &&
		(tree.type.symbol().flags & Modifiers.CASE) == 0);
    }
    */
    boolean isStarApply( Tree.Apply tree ) {
	Symbol params[] = tree.fun.type.valueParams();
	//System.err.println( tree.fun.type.resultType().symbol() );
	return  (tree.args.length == 1)
	    && (tree.type.symbol().flags & Modifiers.CASE) != 0
	    && params.length == 1
	    && (params[ 0 ].flags & Modifiers.REPEATED) != 0;
    }

//////////// generator methods

    public Tree toTree() {
        TreeList ts = new TreeList();
        ts.append( gen.ValDef(root.symbol(), _m.selector ));
        ts.append( gen.ValDef(resultVar,
                              gen.mkDefaultValue(_m.pos, resultVar.info()) ));
        ts.append( gen.If( toTree(root.and),
                           gen.Ident( _m.pos, resultVar ),
                           cf.ThrowMatchError( _m.pos, _m.resultType )));
        /*
            gen.If(
                _m.pos,
                toTree(root.and),
                gen.Ident( _m.pos, resultVar ),
                cf.ThrowMatchError( _m.resultType ));
        */
        return gen.mkBlock(_m.pos, ts.toArray());
    }
    /*
    protected Tree toTree__(PatternNode node) {
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

                          int last = ts.length - 1;
                        ts[ last ] = gen.mkBlock(
                                              new Tree[]{
                                                    gen.Assign(gen.Ident( body[i].pos, resultVar ),
                                                               body[i]),
                                                    gen.mkBooleanLit(body[i].pos, true)
                                              });
                        if (guard[i] != Tree.Empty)
                              ts[ last ] = cf.And(guard[i], ts[ last ]);
                        res = cf.Or(gen.mkBlock(body[i].pos, ts), res);
                    }
                    return res;
                default:
                    throw new ApplicationError();
            }
        return res;
    }
    */
    protected Tree toTree(PatternNode node, Tree selector) {
	//System.err.println("AM.toTree called"+node);
        if (node == null)
            return gen.mkBooleanLit(_m.pos, false);
        switch (node) {
        case SeqContainerPat( _, _ ):
	    return  callSequenceMatcher( node,
					 selector );
        default:
	    return super.toTree( node, selector );
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
		    gen.If(selector.pos,
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

	  SequenceMatcher wordRec = new SequenceMatcher(unit);

	  Matcher m = new Matcher( _m.owner,
				   selector,
				   defs.BOOLEAN_TYPE() );

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
      }

}

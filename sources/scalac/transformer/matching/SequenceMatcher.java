package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.Tree;
import scalac.typechecker.*;
import Tree.*;

//import scala.compiler.printer.TextTreePrinter ; // DEBUGGING\
    //import scala.compiler.printer.XMLAutomPrinter ; // DEBUGGING\

import scalac.transformer.TransMatch.Matcher ;
import scalac.ast.* ;
import scalac.symtab.* ;

import java.util.* ;

import ch.epfl.lamp.util.Position;

/** constructs a matcher for a sequence pattern. plays two roles in
 *  described in design pattern Builder.
 *  is the "Director" for "Builder" class BerrySethi.
 *  is the "Builder" for "Director" class TransMatch.
 */

public class SequenceMatcher extends PatternTool {

      CodeFactory cf;

      Matcher _m;

      Tree pat[];
      Tree body[];

    /*
      public Tree[] getNested( HashMap varMap ) {
            Tree[][] stmsNest = new Tree[varMap.size()][];
            int i = 0;
            int k = 0;
            for( Iterator it = varMap.keySet().iterator() ; it.hasNext(); ) {
                  Tree pat = (Tree) it.next(); // contains variables
                  Symbol v = (Symbol) varMap.get( pat );

                  BindingBerrySethi build = new BindingBerrySethi();
                  NondetWordAutom leftNest = build.automatonFrom( pat,
                                                                  new Integer( 0 ));

                  DetWordAutom dLeftNest  = new DetWordAutom( leftNest );

                  NondetWordAutom rightNest = build.revnfa;

                  Matcher mNest = new Matcher( _m.owner, _m.selector, null );

                  LeftTracerInScala ltisNest =
                        new LeftTracerInScala( dLeftNest,
                                               cf.getElemType( pat.type() ),
                                               mNest,
                                               cf );
                  Tree stmsLeftNest[] = ltisNest.getTrace();

                  Tree selNest = gen.Ident( 0, ltisNest.resultSym );

                  DetWordAutom dRightNest =
                        new DetWordAutom( rightNest, leftNest, dLeftNest);

                  RightTracerInScala rtisNest =
                        new RightTracerInScala( dRightNest, leftNest, mNest,
                                                cf, pat, cf.getElemType(pat.type()));

                  Tree stmsRightNest[] = rtisNest.getStms( gen.Ident( 0, v ) );
                  stmsNest[ i ] = new Tree[ stmsLeftNest.length
                                            + stmsRightNest.length ];

                  System.arraycopy( stmsLeftNest, 0,
                                    stmsNest[ i ], 0, stmsLeftNest.length );
                  System.arraycopy( stmsRightNest, 0, stmsNest[ i ],
                                    stmsLeftNest.length, stmsRightNest.length );
                  k += stmsNest[ i ].length;
                  i++;
            }
            // flatten
            Tree[] res = new Tree[ k ];
            k = 0;
            for( i = 0; i < varMap.size(); i++ ) {
                  System.arraycopy( stmsNest[ i ], 0, res, k, stmsNest[ i ].length );
                  k += stmsNest[ i ].length;
            }
            return res;
      }
    */

    /** translates the det/switching automaton to scala code
     *  precondition: pat.type() corresponds to element type
     */
      Tree addBinderToBody( Tree pat, Tree body ) {
	  /*
            SplitNested spn = new SplitNested( pat, _m.owner, cf );


            pat = spn.flatPat; //

            for( Iterator it = spn.nestedVarToPats.keySet().iterator();
                 it.hasNext(); ){
                  Symbol v = (Symbol) it.next();
                  Tree nestPat = (Tree) spn.nestedVarToPats.get( v );
                  Matcher mNest = new Matcher( _m.owner, gen.Ident(0, v), null );

                  Matcher saveM = _m; _m = mNest;

                  Tree nbody = addBinderToBody( nestPat, body );
                  _m = saveM;
                  body = nbody;
            }
	  */
            Type elementType = cf.getElemType_Sequence( pat.type() );

            BindingBerrySethi build = new BindingBerrySethi();
            NondetWordAutom left =  build.automatonFrom( pat, new Integer(0) );
            NondetWordAutom right = build.revnfa;

            //  - - -> left

            DetWordAutom dLeft  =
                  new DetWordAutom( left );

            Matcher mL = new Matcher( _m.owner, _m.selector, null );

            LeftTracerInScala ltis =
                  new LeftTracerInScala( dLeft, elementType, mL, cf);

            Tree stms[] = ltis.getTrace();

            Tree sel = gen.Ident( 0, ltis.resultSym );

            // <- - - right

            DetWordAutom dRight =
                  new DetWordAutom( right, left, dLeft);

            //System.out.println( "vars: "+vars );
            int j;

            RightTracerInScala rtis =
                  new RightTracerInScala( dRight, left, mL,
                                          cf, pat, elementType );

            Tree stms2[] = rtis.getStms( sel );             // to scala

            // run them, bind

            Tree items[] = new Tree[ stms.length
                                     + stms2.length
                                     + 1 ];

            System.arraycopy( stms, 0, items, 0, stms.length );
            System.arraycopy( stms2, 0, items, stms.length, stms2.length );

            j = stms.length + stms2.length ;
            items[ stms.length + stms2.length ] = body;

            AttributedTreeCopier treeCopier = new AttributedTreeCopier(unit.global,
							     cf.make) ;
		    /* EXPERIMENT, might not work
            TreeCopier treeCopier = new TreeCopier(unit.global,
                                                   unit.global.currentPhase,
                                                   cf.make) {
                        // Substitute symbols
                        public boolean mustSubstituteSymbol(Tree tree) {
                              switch (tree) {
                                    //                              case Select(_,_):
                                      //                              return false;
                              case Ident(Name n):

                                    //System.out.println("tree.symbol:"+tree.symbol());
                                    //return true;
                                    return n.isVariable();

                              default:
                                    return mustCopySymbol(tree);
                              }
                        }
                  };
		    */

            //System.out.println("helpmap");
            //System.out.println( rtis.helpMap2 );
            treeCopier.pushSymbolSubst( rtis.helpMap2 );

            items[ stms.length + stms2.length ] = treeCopier.copy( body );

            return cf.Block( body.pos, items, body.type );
      }

      /** turns body `case pat(x,y,z) => body' into
       **  `{ <runLeft>;<runRight>;body }' if
       *  necessary.
       *  @param body the array containing the bodies of the visitor
       */
      Tree[] addBindersToBodies( Tree body[] ) {
            //System.out.println("addBindersToBodies");
            Tree nbody[] = new Tree[ body.length ];
            for( int i = 0; i < body.length; i++ ) {
                  Integer iI = new Integer( i );

                  if( !CollectVariableTraverser.containsBinding( pat[ i ] ) )
                        {
                              nbody[ i ] = body[ i ]; // no need for binding
                        }
                  else
                        {
                              nbody[ i ] =
                                    addBinderToBody( pat[ i ], body[ i ] );
                        }
            }
            return nbody;
      }
      Type elementType ;

      /** constructs a word recognizer from an array of patterns which
       *  should all be SequencePatterns ( no wildcard * )
       *  precondition: pat.type corresponds to element type
       *  @param _m          Matcher object, holds the result
       *  @param pat         the (Sequence) patterns
       *  @param body        the bodies
       *  @param defaultCase code that is run when nothing matches. may be null, it
       *                     becomes a ThrowMatchError then

       */
      public void construct( Matcher _m,
                             Tree[] pat,
                             Tree[] body,
                             Tree defaultCase,
                             boolean doBinding ) {

            this.pat  = pat;
            this.body = body;
            assert body.length == pat.length;
            this._m = _m;

            this.cf = new CodeFactory( unit, infer /*, _m.pos*/ );

            Type seqType = pat[ 0 ].type();

            elementType = cf.getElemType_Sequence( seqType );

            NondetWordAutom manyNfa[] = new NondetWordAutom[ pat.length ];

            BerrySethi build = new BerrySethi();

            for( int i = 0; i < pat.length; i++ )
                  {
                        manyNfa[ i ] = build.automatonFrom( pat[ i ],
                                                            new Integer( i ));
                  }

            // merge nfas into one if necessary

            NondetWordAutom nfa =
                  (pat.length > 1) ? new NondetWordAutom( manyNfa )
                  : manyNfa[ 0 ];

            DetWordAutom dfa = new DetWordAutom( nfa );

            WordAutomInScala scalaAut = new WordAutomInScala( dfa,
                                                              elementType,
                                                              _m.owner,
                                                              cf);
            scalaAut.translate();

            if( defaultCase == null )
                  defaultCase = cf.ThrowMatchError( Position.NOPOS, _m.resultType );

            Tree newbody[] = doBinding ? addBindersToBodies( body ): body;

            _m.tree = scalaAut.getMatcherSwitch( _m.selector,
                                                 defaultCase,
                                                 newbody,
                                                 _m.resultType );
      } // construct (Matcher, Tree[], Tree[], Tree, boolean )

      /** constructor, invoked  by AlgebraicMatcher
       */
      SequenceMatcher( Unit unit, Infer infer ) {
            super( unit, infer );
            //Symbol predefSym = defs.getModule( defs.SCALA, Names.Predef );
            //Scope predefScope = predefSym.members();
      }
} // class SequenceMatcher

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

    /** translates the det/switching automaton to scala code
     *  precondition: pat.type() corresponds to element type
     */
      Tree addBinderToBody( Tree pat, Tree body ) {

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

            Tree sel = gen.Ident( Position.FIRSTPOS, ltis.resultSym );

            // <- - - right

            DetWordAutom dRight =
                  new DetWordAutom( right, left, dLeft);

            //System.out.println( "vars: "+vars );
            int j;

            final RightTracerInScala rtis =
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

            Transformer treeCloner = new Transformer(unit.global) {
                    public Tree transform(Tree tree) {
                        tree = super.transform(tree);
                        if (tree.hasSymbol()) {
                            Object symbol = rtis.helpMap2.get(tree.symbol());
                            if (symbol != null) tree.setSymbol((Symbol)symbol);
                        }
                        return tree;
                    }
                };
            //System.out.println("helpmap");
            //System.out.println( rtis.helpMap2 );

            items[ stms.length + stms2.length ] = treeCloner.transform( body );
            for( Iterator it = rtis.helpMap2.entrySet().iterator(); it.hasNext(); ) {
                Map.Entry e = (Map.Entry)it.next();
                Symbol key = (Symbol)e.getKey();
                Symbol val = (Symbol)e.getValue();
                val.setInfo( key.type() ); // !!! ?
            }

            return gen.mkBlock( body.pos, items );
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
	  //System.err.println("SequenceMatcher::construct");
            this.pat  = pat;
            this.body = body;
            assert body.length == pat.length;
            this._m = _m;

            this.cf = new CodeFactory( unit, infer, _m.pos );

            Type seqType = pat[ 0 ].type();

            elementType = cf.getElemType_Sequence( seqType );

            NondetWordAutom manyNfa[] = new NondetWordAutom[ pat.length ];

            BerrySethi build = new BerrySethi();

            for( int i = 0; i < pat.length; i++ )
                  {
                        manyNfa[ i ] = build.automatonFrom( pat[ i ],
                                                            new Integer( i ));
			//manyNfa[ i ].print();
                  }

            // merge nfas into one if necessary

            NondetWordAutom nfa =
                  (pat.length > 1) ? new NondetWordAutom( manyNfa )
                  : manyNfa[ 0 ];

	    //nfa.print();

            DetWordAutom dfa = new DetWordAutom( nfa );

            WordAutomInScala scalaAut = new WordAutomInScala( dfa,
                                                              elementType,
                                                              _m.owner,
                                                              cf,
							      unit.global.target == Global.TARGET_JVM );
            scalaAut.translate();

            if( defaultCase == null )
                  defaultCase = cf.ThrowMatchError( Position.FIRSTPOS, _m.resultType );

            Tree newbody[] = doBinding ? addBindersToBodies( body ): body;

            // FIXME - CODE FOR TESTING TUPLE STUFF

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

package scalac.transformer.matching ;

import scalac.*;
import scalac.ast.Tree;
import Tree.*;

//import scala.compiler.printer.TextTreePrinter ; // DEBUGGING\
    //import scala.compiler.printer.XMLAutomPrinter ; // DEBUGGING\

import scalac.ast.* ;
import scalac.symtab.* ;

import java.util.* ;

import scala.tools.util.Position;

/** constructs a matcher for a sequence pattern. plays two roles in
 *  described in design pattern Builder.
 *  is the "Director" for "Builder" class BerrySethi.
 *  is the "Builder" for "Director" class TransMatch.
 */

public class SequenceMatcher extends PatternTool {

    final static Integer IGNORED = new Integer(42);
    CodeFactory cf;

    PartialMatcher _m;

    //Tree pat[];
    //Tree body[];

    BindingBerrySethi bbuild = null;

    /** translates the det/switching automaton to scala code
     *  precondition: pat.type() corresponds to element type
     */
    Tree addBinderToBody( Tree pat, Tree body ) {
        if( bbuild == null )
            bbuild = new BindingBerrySethi(unit);

        Type elementType = cf.getElemType_Sequence( pat.getType() );

        // (a) build *binding* nfa (sequential machine)
        NondetWordAutom left =  bbuild.automatonFrom( pat, IGNORED );
        NondetWordAutom right = bbuild.revnfa;

        //  (b) determinize + translate L

        DetWordAutom dLeft  = new DetWordAutom( left );

        LeftTracerInScala ltis =
            new LeftTracerInScala( dLeft, elementType, _m.owner, _m.selector, cf);

        Tree trace = ltis.getTrace();

        //  (c) determinize + translate R

        DetWordAutom dRight = new DetWordAutom( right, left, dLeft );

        Set seqVars = NondetWordAutom.collectVariables( left );
        //System.out.println("seqVars here are:"+seqVars);
        final RightTracerInScala rtis =
            new RightTracerInScala( dRight, seqVars, _m.owner,
                                    cf, pat, elementType );

        // !!! Tree stms2 = rtis.getStms( theTrace, unit, body );
        // !!! gen.mkBlock_( body.pos, trace, stms2 );
        Tree stms2 = rtis.getStms( trace, unit, body );
        return stms2;
    }

    private NondetWordAutom[] buildNfas( Tree[] pat ) {
        BerrySethi build = new BerrySethi(unit);
        NondetWordAutom manyNfa[] = new NondetWordAutom[ pat.length ];

        for( int i = 0; i < pat.length; i++ ) {
            manyNfa[ i ] = build.automatonFrom( pat[ i ],
                                                new Integer( i ));
            //manyNfa[ i ].print();
        }
        return manyNfa;
    }

    /** constructs a word recognizer from an array of patterns which
     *  should all be SequencePatterns ( no wildcard * )
     *  precondition: pat.type corresponds to element type
     *  @param _m          Matcher object, holds the result
     *  @param pat         the (Sequence) patterns
     *  @param body        the bodies
     *  @param defaultCase code that is run when nothing matches. may be null, it
     *                     becomes a ThrowMatchError then
     *  @param doBinding   flasg that indicates whether variables should be bound
    */
    public void construct( PartialMatcher _m,
                           Tree[] pat,
                           Tree[] body,
                           Tree defaultCase,
                           boolean doBinding ) {
        this._m = _m;
        //this.pat  = pat;
        //this.body = body;
        assert body.length == pat.length;
        if( defaultCase == null )
            defaultCase = cf.ThrowMatchError( cf.pos, _m.resultType );

        this.cf = new CodeFactory( unit, _m.pos );

        Type seqType = pat[ 0 ].getType();
        Type elementType = cf.getElemType_Sequence( seqType );

        // STEP 1 - build nfas for each pattern

        NondetWordAutom manyNfa[] = buildNfas( pat );

        // STEP 2 - recognizing

        // (a) merge nfas into one if necessary
        NondetWordAutom nfa =
            (pat.length > 1) ? new NondetWordAutom( manyNfa )
            : manyNfa[ 0 ];
        //nfa.print();

        // (b) determinize
        DetWordAutom dfa = new DetWordAutom( nfa );

        // (c) translate to scala code
        WordAutomInScala scalaAut = new WordAutomInScala( dfa,
                                                          elementType,
                                                          _m.owner,
                                                          cf,
                                                          unit.global.target == Global.TARGET_JVM );
        scalaAut.translate();

        // STEP 3 - binding

        Tree newbody[];
        if( !doBinding )
            newbody = body;
        else {  // this is done in the body of the matching case
            newbody = new Tree[body.length];
            for( int i = 0; i < body.length; i++ )
                if( !CollectVariableTraverser.containsBinding( pat[ i ] ) )
                    newbody[ i ] = body[ i ]; // no need for binding
                else
                    newbody[ i ] = addBinderToBody( pat[ i ], body[ i ] );
        }

        _m.tree = scalaAut.getMatcherSwitch( _m.selector,
                                             defaultCase,
                                             newbody,
                                             _m.resultType );
    } // construct (PartialMatcher, Tree[], Tree[], Tree, boolean )

      /** constructor, invoked  by AlgebraicMatcher
       */
    SequenceMatcher( CompilationUnit unit ) {
        super( unit );
    }
} // class SequenceMatcher

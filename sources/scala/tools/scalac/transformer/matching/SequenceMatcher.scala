

import scalac._;
import scalac.ast.Tree;
import Tree._;

    //import scala.compiler.printer.XMLAutomPrinter ; // DEBUGGING\

import scalac.ast._ ;
import scalac.symtab._ ;

import java.util._ ;

import scala.tools.util.Position;

package scala.tools.scalac.transformer.matching {
/** constructs a matcher for a sequence pattern. plays two roles in
 *  described in design pattern Builder.
 *  is the "Director" for "Builder" class BerrySethi.
 *  is the "Builder" for "Director" class TransMatch.
 */

class SequenceMatcher(unit: CompilationUnit) extends PatternTool(unit) {

//  Console.println("CONSTR SEQUENCEMATCHER");
    final val IGNORED = new Integer(42);

    var cf: CodeFactory = _;
    var _m: PartialMatcher = _;

    var bbuild: BindingBerrySethi  = null;

    /** collects variables
     * @return all variables bound by this binding nfa
     */
    def collectNfaVariables(nfa: NondetWordAutom): Set = {
      val seqVars = new HashSet();
      var j = 0;
      while(j < nfa.nstates) {
        if( nfa.qbinders( j ) != null )
          seqVars.addAll( nfa.qbinders( j ) );
        j = j + 1
      }
      seqVars;
    }


    /** translates the det/switching automaton to scala code
     *  precondition: pat.type() corresponds to element type
     */
    def addBinderToBody( pat:Tree , body:Tree  ): Tree = {
      if( bbuild == null )
        bbuild = new BindingBerrySethi(unit);

      val elementType = cf.getElemType_Sequence( pat.getType() );

        // (a) build *binding* nfa (sequential machine)
      val left =  bbuild.automatonFrom( pat, IGNORED );
      val right = bbuild.revnfa;

        //  (b) determinize + translate L

      val dLeft  = new DetWordAutom( left );

      val ltis = new LeftTracerInScala( dLeft, elementType, _m.owner, cf, _m.selector);

      val trace = ltis.getTrace();

        //  (c) determinize + translate R

      val dRight = new DetWordAutom( right, left, dLeft );

      val seqVars = collectNfaVariables( left );
        //System.out.println("seqVars here are:"+seqVars);
      val rtis = new RightTracerInScala( dRight, seqVars, _m.owner,
                                        cf, pat, elementType );

      // !!! Tree stms2 = rtis.getStms( theTrace, unit, body );
      // !!! gen.mkBlock_( body.pos, trace, stms2 );
      val stms2 = rtis.getStms( trace, unit, body );
      stms2;
    }

    private def buildNfas( pat:Array[Tree]  ): Array[NondetWordAutom] = {
      val build   = new BerrySethi(unit);
      val manyNfa = new Array[NondetWordAutom]( pat.length );
      var  i = 0;
      while( i < pat.length ) {
        manyNfa( i ) = build.automatonFrom( pat( i ),
                                           new Integer( i ));
        i = i + 1;
        //manyNfa[ i ].print();
      }
      manyNfa;
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
  def construct(_m: PartialMatcher,  pat: Array[Tree] , body: Array[Tree] , defcase1: Tree, doBinding: Boolean  ): Unit = {
    var defaultCase = defcase1;
    this._m = _m;
    //assert body.length == pat.length;
    if( defaultCase == null )
      defaultCase = cf.ThrowMatchError( cf.pos, _m.resultType );

    this.cf = new CodeFactory( unit, _m.pos );

    val seqType = pat( 0 ).getType();
    val elementType = cf.getElemType_Sequence( seqType );

    // STEP 1 - build nfas for each pattern

    val manyNfa = buildNfas( pat );

    // STEP 2 - recognizing

    // (a) merge nfas into one if necessary
    val nfa = if(pat.length > 1) new NondetWordAutom( manyNfa )  else manyNfa( 0 );

    //nfa.print();

    // (b) determinize
    val dfa = new DetWordAutom( nfa );

    // (c) translate to scala code
    val scalaAut =
      new WordAutomInScala( dfa,
                           elementType,
                           _m.owner,
                           cf,
                           unit.global.target == Global.TARGET_JVM );
    scalaAut.translate();

    // STEP 3 - binding

    var newbody:Array[Tree] = _;
    if( !doBinding )
      newbody = body;
    else {  // this is done in the body of the matching case
      newbody = new Array[Tree](body.length);
      var i = 0;
      while(i < body.length) {
        if( !containsBinding( pat( i ) ) )
          newbody( i ) = body( i ); // no need for binding
        else
          newbody( i ) = addBinderToBody( pat( i ), body( i ) );
        i = i + 1;
      }
    }
    _m.tree = scalaAut.getMatcherSwitch( _m.selector,
                                        defaultCase,
                                        newbody,
                                        _m.resultType );
  } // construct (PartialMatcher, Tree[], Tree[], Tree, boolean )

  } // class SequenceMatcher
  }

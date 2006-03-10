/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$

package scala.tools.nsc.matching;

import java.util._ ;

/** constructs a matcher for a sequence pattern. plays two roles in
 *  described in design pattern Builder.
 *  is the "Director" for "Builder" class BerrySethi.
 *  is the "Builder" for "Director" class TransMatch.
 */

trait SequenceMatchers requires TransMatcher  {

  import global._;

  class SequenceMatcher {

  //  Console.println("CONSTR SEQUENCEMATCHER");
  final val IGNORED = new Integer(42);

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
  def addBinderToBody( pat1:Tree , body:Tree  ): Tree = {
    if( bbuild == null )
      bbuild = new BindingBerrySethi;

    val elementType1 = getElemType_Sequence( pat1.tpe );

    // (a) build *binding* nfa (sequential machine)
    val left =  bbuild.automatonFrom( pat1, IGNORED );
    val right = bbuild.revnfa;

    //  (b) determinize + translate L

    val dLeft  = new DetWordAutom( left );

    val ltis = new LeftTracerInScala {
      val dfa =  dLeft;
      val owner =  _m.owner;
      val selector = _m.selector;
      val elementType = elementType1;
    }

    val trace = ltis.getTrace();

    //  (c) determinize + translate R

    val dRight = new DetWordAutom( right, left, dLeft );

    val seqVars1 = collectNfaVariables( left );
    //System.out.println("seqVars here are:"+seqVars);
    val rtis = new RightTracerInScala {
      val dfa = dRight;
      val owner = _m.owner;
      val pat = pat1;
      val seqVars = seqVars1;
      val elementType = elementType1;
    }

    // !!! Tree stms2 = rtis.getStms( theTrace, cunit, body );
    // !!! gen.mkBlock_( body.pos, trace, stms2 );
    val stms2 = rtis.getStms( trace, cunit, body );
    stms2;
  }

    private def buildNfas( pat:scala.List[Tree]  ): Array[NondetWordAutom] = {
      val build   = new BerrySethi;
      val manyNfa = new Array[NondetWordAutom]( pat.length );
      var i = 0;
      val it = pat.elements;
      while( i < pat.length ) {
        manyNfa( i ) = build.automatonFrom( it.next, new Integer( i ));
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
  def construct(_m: PartialMatcher,  pat: scala.List[Tree] , body: scala.List[Tree] , defcase1: Tree, doBinding: Boolean  ): Unit = {
    var defaultCase = defcase1;
    this._m = _m;
    //assert body.length == pat.length;
    if( defaultCase == null )
      defaultCase = ThrowMatchError( _m.pos, resultType );

    val seqType = pat( 0 ).tpe;
    val elementType1 = getElemType_Sequence( seqType );

    // STEP 1 - build nfas for each pattern

    val manyNfa = buildNfas( pat );

    // STEP 2 - recognizing

    // (a) merge nfas into one if necessary
    val nfa = if(pat.length > 1)
      new NondetWordAutom( manyNfa )
              else
                manyNfa( 0 );

    //nfa.print();

    // (b) determinize
    val dfa1 = new DetWordAutom( nfa );

    // (c) translate to scala code
    val scalaAut =
      new WordAutomInScala{
        val dfa = dfa1;
        val owner = _m.owner;
        val optim = settings.target == "jvm";
        val elementType = elementType1;
      }
    scalaAut.translate();

    // STEP 3 - binding

    var newbody: scala.List[Tree] = Nil;
    if( !doBinding )
      newbody = body;
    else {  // this is done in the body of the matching case
      var i = 0;
      while(i < body.length) {
        if( !containsBinding( pat( i ) ) )
          newbody = body( i ) :: newbody; // no need for binding
        else
          newbody = addBinderToBody( pat( i ), body( i ) ) :: newbody;
        i = i + 1;
      }
      newbody = newbody.reverse;
    }
    _m.tree = scalaAut.getMatcherSwitch( _m.selector,
                                        defaultCase,
                                        newbody );
  } // construct (PartialMatcher, Tree[], Tree[], Tree, boolean )

  } // class SequenceMatcher
}

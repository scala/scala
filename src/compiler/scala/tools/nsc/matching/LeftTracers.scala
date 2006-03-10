/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$

package scala.tools.nsc.matching;

import java.util._ ;

import scala.tools.nsc.util.Position;

trait LeftTracers requires TransMatcher {

import global._;

abstract class LeftTracerInScala extends Autom2Scala {

  val selector: Tree;
  val elementType: Type;

    /** symbol of the accumulator ( scala.SequenceList )
     */
    var accumSym: Symbol   = _;
    var accumType: Type    = _;
    var accumTypeArg: Type =_ ;

    def _accumType(elemType: Type): Type =  {
        SeqTraceType( elemType );
    }

    protected def initializeSyms(): Unit = {
      this.funSym =  owner.newLabel( pos, fresh.newName( "left" ));

      this.iterSym = owner.newVariable( pos, fresh.newName( "iter" ))
      .setInfo( _seqIterType( elementType ) ) ;

      this.stateSym = owner.newVariable( pos, fresh.newName( "q" ))
      .setInfo( definitions.IntClass.info ) ;

      this.accumType = _accumType( elementType );
      this.accumTypeArg = accumType.typeArgs( 0 );
      this.accumSym = owner.newVariable( pos,                  // accumulator
                                        fresh.newName( "acc" ))
      .setInfo( accumType );

      //this.funSym
      //    .setInfo( new MethodType( new Symbol[] {
      //        accumSym, iterSym, stateSym},
      //                                   accumType));

      this.funSym.setInfo(
        MethodType(
          scala.List (  // dummy symbol MethodType
            definitions.IntClass.info,
            accumType
          ),
          accumType)
      );

      //funSym.newValueParameter( pos, fresh.newName( "q" ))
      //.setInfo(definitions.IntClass.info),
      //funSym.newValueParameter( pos, fresh.newName( "acc" ))
      //.setInfo( accumType ) ),
      // accumType)); // result type = List[T]

      this.resultSym = owner.newVariable(pos, fresh.newName("trace"))
      .setInfo( accumType ) ;

      this.curSym = owner.newVariable( pos, "cur" )
      .setInfo( elementType );

      this.hasnSym = owner.newVariable( pos, nme.hasNext )
      .setInfo( definitions.BooleanClass.info );

    }

  /* should throw an exception here really, e.g. MatchError
   */
  override def code_fail() = Ident( accumSym );

  /** returns translation of transition with label from i.
     *  returns null if there is no such transition(no translation needed)
     */
    override def code_delta(i: Int, label: Label): Tree = {
      val target = dfa.delta( i, label );

      /*
       System.out.println("LeftTracer:calling dfa.delta("+i+","+label+")");
       System.out.println("result: "+target);
       */
      if( target == null )
        null;
      else {
        // (optimization) that one is a dead state (does not make sense for tracer)
        /*
          if( target == dfa.nstates - 1 )
          return code_fail();
        */

        /*
          Tree newAcc = newSeqTraceCons(new Integer(i),
          currentElem(),
          _ref( accumSym ));
        */
        val hd = gen.mkNewPair( Literal(i), currentElem() );

        val newAcc = gen.mkNewCons(hd, Ident(accumSym ));

        //return callFun( new Tree[] { newAcc , _iter(), mkIntLit( pos, target )} );
        callFun( scala.List( Literal(target.intValue() ), newAcc ) );
      }
    }


  def code_body(): Tree = {

    var body = code_error(); // never reached at runtime.

    // state [ nstates-1 ] is the dead state, so we skip it

    //`if( state == q ) <code_state> else {...}'
    var i = dfa.nstates() - 2;
    while (i >= 0) {
      body = code_state(i, body);
      i = i - 1;
    }
    loadCurrentElem(body);
  }

    /** return code for state i of the dfa SAME AS IN SUPER, ONLY SINK IS GONE
     */
  def code_state(i: Int, elseBody: Tree): Tree = {

    var runFinished: Tree = null; // holds result of the run
    var finalSwRes: Int = 0;

    runFinished = run_finished(i);

    var stateBody: Tree = null ; // action(delta) for one particular label/test

    // default action (fail if there is none)

    stateBody = code_delta( i, DefaultLabel());

    if( stateBody == null )
      stateBody = code_fail();
    // transitions of state i

    val trans = dfa.deltaq( i );
    val labs  = dfa.deltaq( i ).keySet().iterator();
    while(labs.hasNext()) {
      val label = labs.next();
      val next = trans.get( label ).asInstanceOf[Integer];

      val  action = code_delta( i, label.asInstanceOf[Label] );

      if( action != null ) {
        stateBody = If( currentMatches(label.asInstanceOf[Label]),
                       action,
                       stateBody);
      }
    }
    stateBody = If(Negate(Ident(hasnSym)),
                   runFinished,
                   stateBody );
    If( Equals( _state(), Literal( i )),
       stateBody ,
       elseBody );
  }

  def  getTrace(): Tree = {

    initializeSyms();

   Block(scala.List(
      ValDef( iterSym, newIterator( selector )),
      ValDef( stateSym, Literal( 0 ) ),
      ValDef( accumSym, gen.mkNil /*mkNil( pos )*/),
      ValDef( resultSym,
                 LabelDef( this.funSym,
                              scala.List (
                                stateSym,
                                accumSym
                              ), code_body() /* code_body_new ? */ ))
   ),
         Ident( resultSym ));
  }

  // calling the AlgebraicMatcher here
  override def _cur_match(pat: Tree): Tree  = {
    //return mkBooleanLit(pos, true);

    //System.out.println("calling algebraic matcher on type:" + pat.type);

    val m = new PartialMatcher {
      val owner = LeftTracerInScala.this.owner;
      val selector = currentElem();

      // result type definitions.BooleanClass.info );
    }

    pat match {
        case Sequence(pats) if containsBinding(pat) =>
          //scala.Predef.error("should not happen?!");
          null; // Literal(true); ?!
      case _ =>
        am.construct(m, scala.List (
          CaseDef( pat, Literal( true )),
          CaseDef( Ident( nme.WILDCARD ), Literal(false)) ),
                     false);
      am.toTree();
    }
  }


  /** return the accumulator + last state
   */
  override def run_finished(state: Int): Tree = {
    val hd = gen.mkNewPair( Literal(state), EmptyTree);
    //System.err.println(hd.type);
    gen.mkNewCons(hd, Ident( accumSym ));
/*
    mkNewCons(pos,
                  accumTypeArg,
                  hd,
                  Ident( accumSym ));
*/
  }

} // TracerInScala
} // LeftTracers

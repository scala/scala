import scalac._;
import scalac.ast._;
import scalac.symtab._;
import Tree._;
import scalac.util.Names;

import java.util._ ;

import scala.tools.util.Position;
//import scalac.transformer.matching._ ;
import scalac.transformer.matching.CodeFactory ;
import scalac.transformer.matching.DetWordAutom ;
import scalac.transformer.matching.Label ;
import scalac.transformer.matching.PartialMatcher ;
package scala.tools.scalac.transformer.matching {

class LeftTracerInScala(dfa: DetWordAutom, elementType: Type, owner: Symbol, cf: CodeFactory, val selector: Tree )
extends TracerInScala( dfa, elementType, owner, cf ) {

  final def defs = cf.defs;

    /** symbol of the accumulator ( scala.SequenceList )
     */
    var accumSym: Symbol   = _;
    var accumType: Type    = _;
    var accumTypeArg: Type =_ ;

    def _accumType(elemType: Type): Type =  {
        cf.SeqTraceType( elemType );
    }


    protected def initializeSyms(): Unit = {
      this.funSym =  owner.newLabel( pos,
                                    cf.fresh.newName( "left" ));

      this.iterSym = owner.newVariable( pos,
                                       Modifiers.MUTABLE,
                                       cf.fresh.newName( "iter" ))
      .setType( cf._seqIterType( elementType ) ) ;

      this.stateSym = owner.newVariable( pos,
                                        Modifiers.MUTABLE,
                                        cf.fresh.newName( "q" ))
      .setType( defs.int_TYPE() ) ;

      this.accumType = _accumType( elementType );
      this.accumTypeArg = accumType.typeArgs()( 0 );
      this.accumSym = owner.newVariable( pos,                  // accumulator
                                        Modifiers.MUTABLE,
                                        cf.fresh.newName( "acc" ))
      .setType( accumType );

      //this.funSym
      //    .setType( new Type.MethodType( new Symbol[] {
      //        accumSym, iterSym, stateSym},
      //                                   accumType));

      this.funSym
      .setType( new Type.MethodType( Predef.Array[Symbol] (  // dummy symbol MethodType
        funSym.newVParam( pos, 0, cf.fresh.newName( "q" ), defs.int_TYPE()),
        funSym.newVParam( pos, 0, cf.fresh.newName( "acc" ), accumType ) ),
                                    accumType)); // result type = List[T]

      this.resultSym = owner.newVariable(pos,
                                         0,
                                         cf.fresh.newName("trace"))
      .setType( accumType ) ;

      this.curSym = owner.newVariable( pos, 0, Names.cur )
      .setType( elementType );

      this.hasnSym = owner.newVariable( pos, 0, Names.hasNext )
      .setType( defs.boolean_TYPE() );

    }

  /* should throw an exception here really, e.g. MatchError
   */
  override def code_fail() = {
     gen.Ident( accumSym.pos, accumSym );
  }

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
          Tree newAcc = cf.newSeqTraceCons(new Integer(i),
          currentElem(),
          _ref( accumSym ));
        */
        val hd = cf.newPair( gen.mkIntLit(cf.pos, i), currentElem() );
        val newAcc = gen.mkNewCons( cf.pos,
                                   accumTypeArg,
                                   hd,
                                   gen.Ident( cf.pos, accumSym ));

        //return callFun( new Tree[] { newAcc , _iter(), gen.mkIntLit( cf.pos, target )} );
        callFun( Predef.Array[Tree]( gen.mkIntLit( cf.pos, target.intValue() ), newAcc ) );
      }
    }


  def code_body(): Tree = {

    var body = code_error(); // never reached at runtime.

    // state [ nstates-1 ] is the dead state, so we skip it

    //`if( state == q ) <code_state> else {...}'
    var i = dfa.nstates()-2;
    while(i >= 0) {
      body = code_state( i, body );
      i = i - 1;
    }
    loadCurrentElem( body );
  }

    /** return code for state i of the dfa SAME AS IN SUPER, ONLY SINK IS GONE
     */
  def code_state(i: Int, elseBody: Tree): Tree  = {

    var runFinished: Tree = _; // holds result of the run
    var finalSwRes: Int = 0;

    runFinished = run_finished( i );

    var stateBody: Tree = _ ; // action(delta) for one particular label/test

    // default action (fail if there is none)

    stateBody = code_delta( i, Label.DefaultLabel);

    if( stateBody == null )
      stateBody = code_fail();
    // transitions of state i

    val trans = (dfa.deltaq.asInstanceOf[Array[HashMap]])( i );
    val labs = (dfa.deltaq( i ).asInstanceOf[HashMap]).keySet().iterator();
    while(labs.hasNext()) {
      val label = labs.next();
      val next = trans.get( label ).asInstanceOf[Integer];

      val  action = code_delta( i, label.asInstanceOf[Label] );

      if( action != null ) {
        stateBody = gen.If( currentMatches(label.asInstanceOf[Label]),
                           action,
                           stateBody);
      }
    }
    stateBody = gen.If( cf.Negate( gen.Ident( cf.pos, hasnSym )),
                       runFinished,
                       stateBody );
    gen.If( cf.Equals( _state(), gen.mkIntLit(cf.pos, i )),
           stateBody ,
           elseBody );
  }

  def  getTrace(): Tree = {

    initializeSyms();

    cf.gen.mkBlock( cf.pos, Predef.Array[Tree] (
      gen.ValDef( iterSym, cf.newIterator( selector, selector.getType() )),
      gen.ValDef( stateSym, gen.mkIntLit( cf.pos, 0) ),
      gen.ValDef( accumSym, gen.mkNil( cf.pos )),
      gen.ValDef( resultSym,
                 gen.LabelDef( this.funSym,
                              Predef.Array[Ident] (
                                gen.Ident( pos, stateSym ),
                                gen.Ident( pos, accumSym )
                              ), code_body() /* code_body_new ? */ ))
    ),
                   gen.Ident( cf.pos, resultSym ));
  }

  // calling the AlgebraicMatcher here
  override def _cur_match(pat: Tree): Tree  = {
    //return gen.mkBooleanLit(cf.pos, true);

    //System.out.println("calling algebraic matcher on type:"+pat.type);

    val m = new PartialMatcher( owner,
                               currentElem(),
                               defs.boolean_TYPE() );

    val res1 = if(containsBinding( pat )) {
      pat.match {
        case Sequence(pats) =>
          gen.mkBooleanLit(cf.pos, true);
        case _ =>
          null
      }
    } else null;

    if (res1 == null) {

      am.construct( m, Predef.Array[Tree] (
        cf.gen.CaseDef( pat,
                       gen.mkBooleanLit( cf.pos, true )),
        cf.gen.CaseDef( cf.gen.Ident( pat.pos, defs.PATTERN_WILDCARD ),
                       gen.mkBooleanLit( cf.pos, false)) ),
                   false);
      val res = am.toTree();
      // debugprint ?
      res;
    } else null
  }


    /** return the accumulator + last state
     */
    override def run_finished(state: Int): Tree  = {
      val hd = cf.newPair( gen.mkIntLit( cf.pos, state ),
                          gen.mkDefaultValue(cf.pos,
                                             elementType));
      //System.err.println(hd.type);
      gen.mkNewCons(  cf.pos,
                    accumTypeArg,
                    hd,
                    gen.Ident( cf.pos, accumSym ));
    }

}
}

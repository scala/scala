package scala.tools.nsc.matching ;

//import java.util._ ;

import scala.tools.nsc.util.Position;

trait Autom2 requires TransMatcher {

  import global._;

  /**  @param owner owner of the pattern matching expression
   */
  abstract class Autom2Scala {

    val dfa: DetWordAutom;
    val owner: Symbol;

    protected var optimize = true;

    final val FAIL = -1;

    /** symbol of the matcher DefDef or Label */
    var funSym:Symbol = _;

    /** symbol of the iterator ( scala.SequenceIterator ) */
    var iterSym: Symbol = _;

    /** symbol of the switching result ( scala.Int ) */
    var resultSym: Symbol = _;

    /** symbol of the state variable ( scala.Int ) */
    var stateSym: Symbol = _;

    /** symbol of variable holding current label */
    var curSym: Symbol = _;

    /** symbol of boolean variable that indicates we have not reached end of sequence */
    var hasnSym: Symbol = _;

    val am = new AlgebraicMatcher();

    var pos: Int = Position.FIRSTPOS;

    val elementType: Type;

    def funRetType(): Type = {
      funSym.tpe match {
        case MethodType( _, retType )=> retType;
        case _ => scala.Predef.error("quoi?");
      }
    }

    def callFun(args: List[Tree]): Tree = Apply(Ident(funSym),args);

    // overridden in RightTracerInScala
    def loadCurrentElem(body: Tree): Tree = {
      Block(
        List(
          ValDef(this.hasnSym, _hasNext( _iter() ) ),
          ValDef(this.curSym, If(Ident( hasnSym ),
                                 _next( _iter() ),
                                 EmptyTree))
        ),
        body
      );
    }

    /** bug ?? */
    def currentElem() = { Ident( curSym ) }

    def currentMatches(label: Label): Tree = {
      label match {
        case TreeLabel( pat ) =>
          _cur_match( pat );
        case SimpleLabel(lit: Literal) =>
          Equals( currentElem(), lit );
        case _ => // cannot happen
          scala.Predef.error("expected either algebraic or simple label:"+label);
      }
    }

    //
    // translation of automata to scala code
    //


    /** `[switchResult]' */
    def _swres(): Tree = { Ident( resultSym );}

    /** `<state>' param */
    def _state(): Tree = { Ident( stateSym ); }

    /** `<iterator>' param */
    def  _iter(): Tree = { Ident( iterSym );  }

    /** simple optimization: if we are in a sink state, stop traversing sequence
     */
    def stateWrap(i: Int): Tree = {
      if( dfa.isSink( i ))
        run_finished( i ); // state won't change! optimization
      else
        If( Negate( Ident( hasnSym )),
           run_finished( i ),
           code_state_NEW( i ));
    }

    /** body of the matcherDefFun
     */
    def code_body_NEW(): Tree  = {
      var cases: List[CaseDef] = Nil;

      //val tags   = new Array[Int](dfa.nstates());
      //val bodies = new Array[Tree](dfa.nstates());
      var i = 0; while (i < dfa.nstates()) {
        cases = CaseDef( Literal(i), stateWrap(i)) :: cases;
        i = i + 1;
      }
      //if( optimize )
      loadCurrentElem( Match( _state(), cases ));

      /*
       Tree res = code_error();
       for( int i = dfa.nstates-2; i>= 0; i-- )
       res = gen.If( Equals( _state(), gen.mkIntLit( pos, i )),
       bodies[ i ] ,
       res );

       return loadCurrentElem( res );
       */
    }

    /* calling the (AlgebraicMatcher)PatternMatcher here */
    def _cur_match(pat: Tree): Tree  = {
      val m:  PartialMatcher = new PartialMatcher {
        val owner = Autom2Scala.this.funSym;   /* owner*/
        val selector = currentElem(); /* root */
                                 /* defs.boolean_TYPE() restype */
      };

      am.construct( m, List (
        CaseDef(                 pat, Literal(true)),
        CaseDef( Ident(nme.WILDCARD), Literal(false))
      ),
                   false);
      am.toTree();
    }

    // @todo should be abstract
    def code_delta( i:Int, label: Label): Tree = {
        scala.Predef.error("should not happen");
    }

    /** some error happened which is due to bug in translation/automaton
     */
    final def code_error(): Tree = {
        ThrowMatchError( pos , funRetType() );
    }

    def code_fail(): Tree = {
      Literal( FAIL ); //gen.mkIntLit(Position.FIRSTPOS, FAIL );
    }

    /** code for the return value of the automaton translation
     */
    def run_finished(state: Int): Tree = {
      if( dfa.isFinal( state ))
        Literal(
          dfa.finals.get( new Integer( state ) ).asInstanceOf[Integer]
          .intValue()
        )
      else
        Literal( FAIL );
    }

    def code_state_NEW(i: Int): Tree = {
      var stateBody = code_delta( i, DefaultLabel() );
      if( stateBody == null )
        stateBody = code_fail();
      val trans = dfa.deltaq( i );

      val  labs = dfa.labels().iterator();
      while(labs.hasNext()) {
        val label  = labs.next();
        val next   = trans.get( label ).asInstanceOf[Integer];
        val action = code_delta( i, label.asInstanceOf[Label] );
        if( action != null ) {
          /*assert*/ //stateBody != null : "stateBody is null";
          stateBody = If( currentMatches(label.asInstanceOf[Label] ),
                         action,
                         stateBody);
        }
      }
      stateBody;
    }
  }
}

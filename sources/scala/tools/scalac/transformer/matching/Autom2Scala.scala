/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{Global => scalac_Global};
import scalac._;
import scalac.ast._;
import scalac.symtab._;
import scalac.util._;       // Names

import scalac.transformer.{ OwnerTransformer
                           => scalac_transformer_OwnerTransformer };

import Tree._;

import java.util._ ;

import scala.tools.util.Position;

package scala.tools.scalac.transformer.matching {

  /**  @param owner owner of the pattern matching expression
   */
  class Autom2Scala(val dfa: DetWordAutom, val elementType: Type, val owner: Symbol, val cf: CodeFactory)  {

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

    val am = new AlgebraicMatcher( cf.unit );

    var pos: Int = Position.FIRSTPOS;

    def funRetType(): Type = {
      funSym.getType().match {
        case Type.MethodType( _, retType )=>
          retType;
        case _ => throw new RuntimeException();
      }
    }
    final def gen = cf.gen;

    def callFun(args: Array[Tree]): Tree = {
      gen.mkApply_V(gen.Ident(pos, funSym), args);
    }

    // overridden in RightTracerInScala
    def loadCurrentElem(body: Tree): Tree = {
      gen.mkBlock( Predef.Array[Tree] (
        cf.gen.ValDef(this.hasnSym,
                      cf._hasNext( _iter() ) ),
        cf.gen.ValDef(this.curSym,
                      gen.If(gen.Ident( pos, hasnSym ),
                             cf._next( _iter() ),
                             gen.mkDefaultValue(cf.pos,curSym.getType())))
      ),
                  body );
    }

    /** bug ?? */
    def currentElem() = { gen.Ident( cf.pos, curSym ).setType( curSym.getType() ); }

    def currentMatches(label: Label): Tree = {
      label.match {
        case TreeLabel( pat ) =>
          _cur_match( pat );
        case SimpleLabel(lit: Tree.Literal) =>
            cf.Equals( currentElem(), lit );
        case _ => // cannot happen
          throw new ApplicationError("expected either algebraic or simple label:"+label);
      }
    }

    //
    // translation of automata to scala code
    //


    /** `[switchResult]' */
    def _swres(): Tree = { gen.Ident( pos, resultSym );}

    /** `<state>' param */
    def _state(): Tree = { gen.Ident( pos, stateSym ); }

    /** `<iterator>' param */
    def  _iter(): Tree = { gen.Ident( pos, iterSym );  }

    /** simple optimization: if we are in a sink state, stop traversing sequence
     */
    def stateWrap(i: Int): Tree = {
      if( dfa.isSink( i ))
        run_finished( i ); // state won't change! optimization
      else
        gen.If( cf.Negate( gen.Ident( pos, hasnSym )),
               run_finished( i ),
               code_state_NEW( i ));
    }

    /** body of the matcherDefFun
     */
    def code_body_NEW(): Tree  = {
        val tags   = new Array[Int](dfa.nstates());
        val bodies = new Array[Tree](dfa.nstates());
        var i = 0; while (i < dfa.nstates()) {
          tags( i )   = i;
          bodies( i ) = stateWrap( i );
          i = i + 1;
        }
        //if( optimize )
        loadCurrentElem( gen.Switch( _state(),
                                    tags,
                                    bodies,
                                    code_error(), // cannot happen
                                    funRetType()));
      /*
       Tree res = code_error();
       for( int i = dfa.nstates-2; i>= 0; i-- )
       res = gen.If( cf.Equals( _state(), gen.mkIntLit( cf.pos, i )),
       bodies[ i ] ,
       res );

       return loadCurrentElem( res );
       */
    }

    /* calling the (AlgebraicMatcher)PatternMatcher here */
    def _cur_match(pat: Tree): Tree  = {
      val m = new PartialMatcher( this.funSym,   /* owner*/
                                 currentElem(), /* root */
                                 cf.defs.boolean_TYPE() /* restype */);

      am.construct( m, Predef.Array[Tree] (
        cf.gen.CaseDef( pat,
                       gen.mkBooleanLit( pat.pos, true )),
        cf.gen.CaseDef( cf.gen.Ident(pat.pos, cf.defs.PATTERN_WILDCARD),
                       gen.mkBooleanLit( pat.pos, false )) ),
                   false);
      am.toTree();
    }

    // @todo should be abstract
    def code_delta( i:Int, label: Label): Tree = {
        throw new RuntimeException();
    }

    /** some error happened which is due to bug in translation/automaton
     */
    final def code_error(): Tree = {
        cf.ThrowMatchError( pos, funRetType() );
    }

    def code_fail(): Tree = {
      gen.mkIntLit(Position.FIRSTPOS, FAIL );
    }

    /** code for the return value of the automaton translation
     */
    def run_finished(state: Int): Tree = {
      if( dfa.isFinal( state ))
        gen.mkIntLit(Position.FIRSTPOS, dfa.finals.get( new Integer( state ) ).asInstanceOf[Integer].intValue() );
      else
        gen.mkIntLit( Position.FIRSTPOS, FAIL );
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
          stateBody = gen.If( currentMatches(label.asInstanceOf[Label] ),
                             action,
                             stateBody);
        }
      }
      stateBody;
    }
  }
}

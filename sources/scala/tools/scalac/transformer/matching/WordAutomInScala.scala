/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$



import scala.tools.util.Position;

import scalac._;
import scalac.ast.Tree;
import scalac.ast.TreeGen;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.Modifiers; // test
//import scalac.typechecker.*;
import Tree._;

import java.util._;
import scala.tools.scalac.util.NewArray;

import scalac.transformer.matching.CodeFactory;
import scalac.transformer.matching.DetWordAutom;
import scalac.transformer.matching.Label;
import scalac.transformer.matching.PartialMatcher;
import scalac.util.Names;

package scala.tools.scalac.transformer.matching {
  /** translates a recognizer to scala code
   */

    /** constructor
     * @param dfa         the dfa that is to be translated
     * @param elementType type of the objects in the sequence
     * @param owner       the owner of the pattern match
     * @param cf          code factory
     * @param optim       flag that indicates whether to optimize
     * @return            an object that translates the dfa
     */
  class WordAutomInScala(dfa: DetWordAutom, elementType: Type, owner: Symbol, cf: CodeFactory, optim: Boolean )
  extends Autom2Scala(dfa, elementType, owner, cf) {

    final def defs = cf.defs;
    this.optimize = this.optimize && optim;
    var theDefDef: Tree = _ ;

    def getMatcherSwitch(selector: Tree, failTree: Tree, body: Array[Tree], resultType: Type ): Tree = {

      var result: Tree = _;

      /*
       boolean insane = true; // if you set this to false, you get some VerifyErrors
       // seems fixed
       if( insane ) { // cascading ifs

       Tree cond[] = new Tree[body.length];
       for( int i = body.length - 1; i >= 0; i-- ) {
       cond[i] = cf.Equals(_swres(), gen.mkIntLit( cf.pos, i ));
       }
       result = cf.Switch( cond, body, failTree );

       } else {        // real switch
       */
      val tags = new Array[int](body.length);
      var i = body.length - 1;
      while( i >= 0 ) {
        tags(i) = i;
        i = i - 1
      }
      result = gen.Switch( _swres(), tags, body, failTree );

      //}

      result = cf.gen.mkBlock( cf.pos,
                              NewArray.Tree (
                                gen.ValDef( iterSym, cf.newIterator( selector )),
                                gen.ValDef( stateSym, gen.mkIntLit( cf.pos, 0) ),
                                gen.ValDef( resultSym, theDefDef )),
                              result );
      //unit.global.debugPrinter.print( result );
      result;
    }

    protected def initializeSyms(): Unit =  { // TEST

      this.funSym = owner.newLabel( pos,
                                   cf.fresh.newName( "matcher" ));

      this.iterSym = owner.newVariable( pos,
                                       Modifiers.MUTABLE,
                                       cf.fresh.newName("iter"))
      .setType( cf._seqIterType( elementType ) ) ;

      this.stateSym = owner.newVariable( pos,
                                        Modifiers.MUTABLE,
                                        cf.fresh.newName("q"))
      .setType( defs.int_TYPE() ) ;

      this.resultSym = owner.newVariable( pos,
                                         Modifiers.MUTABLE,
                                         cf.fresh.newName("swRes"))
      .setType( defs.int_TYPE() ) ;

      this.funSym
      .setType( new Type.MethodType( Predef.Array[Symbol] (
        funSym.newVParam( pos, 0, cf.fresh.newName("q"), defs.int_TYPE())
      ), defs.int_TYPE() ));

      this.curSym = owner.newVariable( pos, 0, Names.cur )
      .setType( elementType );

      this.hasnSym = owner.newVariable( pos, 0, Names.hasNext )
      .setType( defs.boolean_TYPE() );

    }

    /** code for the return value of the automaton translation
     */
    override def run_finished( state: Int): Tree = { // T E S T
      if( dfa.isFinal( state ))
        gen.mkIntLit(Position.FIRSTPOS, dfa.finals.get( new Integer( state ) ).asInstanceOf[Integer].intValue() );
      else
        gen.mkIntLit( Position.FIRSTPOS, FAIL );
    }


    // calling the /*AlgebraicMatcher*/PatternMatcher here
    override def _cur_match(pat: Tree): Tree = { // TE ST
      val m = new PartialMatcher( this.owner,   /* owner*/
                                 currentElem(), /* root */
                                 defs.boolean_TYPE() /* restype */);

      am.construct( m, Predef.Array[Tree] (
        cf.gen.CaseDef( pat,
                       gen.mkBooleanLit( pat.pos, true )),
        cf.gen.CaseDef( cf.gen.Ident(pat.pos, defs.PATTERN_WILDCARD),
                       gen.mkBooleanLit( pat.pos, false )) ),
                   false);
      am.toTree();
    }

    /** do the translation
     */
    def translate(): Unit = {
      initializeSyms();
      val tb = code_body_NEW();
      //theDefDef = gen.DefDef(this.funSym, tb);
      theDefDef = gen.LabelDef(this.funSym, Predef.Array[Ident] ( /*(Ident)_iter(),*/ _state().asInstanceOf[Ident] ), tb);
    }

    /** ...
     * @return returns translation of transition with label from i.
     * returns null if there is no such transition
     * (no translation needed)
     */
    override def  code_delta(i: Int, label: Label): Tree = {
      val target = dfa.delta(i, label);

      if (target == null)
        label.match  {
          case Label.DefaultLabel =>
            code_error(); // this may not happen !
          case _ =>
            null; // not good
        }
      else if (target.intValue() == dfa.nstates() - 1) // that one is a dead state
        code_fail();
      else
        callFun(Predef.Array[Tree]( gen.mkIntLit( cf.pos, target.intValue() )));
    }

  }
}

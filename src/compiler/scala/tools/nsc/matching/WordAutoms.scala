/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$

package scala.tools.nsc.matching;

import java.util._ ;

import scala.tools.nsc.util.Position;

trait WordAutoms requires TransMatcher {

  import global._ ;
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
abstract class WordAutomInScala extends Autom2Scala {

  val optim: Boolean;

  this.optimize = this.optimize && optim;

  var theLabelDef: Tree = _ ;

  def getMatcherSwitch(selector: Tree, failTree: Tree, body: scala.List[Tree] /*, resultType: Type*/ ): Tree = {

    var result: Tree = null;
    var ncases: scala.List[CaseDef] = Nil;
    val it = body.reverse.elements;
    //val tags = new Array[int](body.length);
    var i = body.length - 1;
    while( i >= 0 ) {
      //tags(i) = i;
      ncases = CaseDef(Literal(i), it.next) :: ncases;
      i = i - 1
      }

      ncases= ncases::: CaseDef(Ident(nme.WILDCARD),failTree) :: Nil;
      result = Match( _swres(), ncases );

      //}

      result =
        Block(
          scala.List (
            ValDef( iterSym, newIterator( selector.duplicate )),
            ValDef( stateSym, Literal(0) ),
            ValDef( resultSym, theLabelDef )),
          result
        );
      //unit.global.debugPrinter.print( result );
      result;
    }

    protected def initializeSyms(): Unit =  { // TEST

      this.funSym = owner.newLabel( pos, fresh.newName( "matcher" ));

      this.iterSym = owner.newVariable( pos, fresh.newName("iter"))
      .setInfo( _seqIterType( elementType ) ) ;

      this.stateSym = owner.newVariable( pos, fresh.newName("q"))
      .setInfo( definitions.IntClass.info ) ;

      this.resultSym = owner.newVariable( pos, fresh.newName("swRes"))
      .setInfo( definitions.IntClass.info ) ;

      this.funSym.setInfo( MethodType(scala.List (definitions.IntClass.info),
                                          definitions.IntClass.info ));

      this.curSym = owner.newVariable( pos, "cur" /*Names.cur*/ )
      .setInfo( elementType );

      this.hasnSym = owner.newVariable( pos, nme.hasNext )
      .setInfo( definitions.BooleanClass.info );

    }

    /** code for the return value of the automaton translation
     */
    override def run_finished(state: Int): Tree = { // T E S T
      if( dfa.isFinal(state))
        Literal(dfa.finals.get(new Integer(state)).asInstanceOf[Integer].intValue());
      else
        Literal(FAIL);
    }


  // calling the /*AlgebraicMatcher*/PatternMatcher here
  override def _cur_match(pat: Tree): Tree = { // TE ST
    val m = new PartialMatcher {
      val owner = WordAutomInScala.this.owner;   /* owner*/
      val selector = currentElem(); /* root */
      // restyp definitions.BooleanClass.info /* restype */);
    }

    am.construct( m, scala.List (
      CaseDef( pat, Literal( true )),
      CaseDef( Ident(definitions.PatternWildcard), Literal( false )) ),
                 false);
    am.toTree();
  }

  /** do the translation
   */
  def translate(): Unit = {
    initializeSyms();
    val tb = code_body_NEW();
    //theLabelDef = gen.DefDef(this.funSym, tb);
    theLabelDef = LabelDef(this.funSym, scala.List( stateSym ), tb);
  }

  /** ...
   * @return returns translation of transition with label from i.
   * returns null if there is no such transition
   * (no translation needed)
   */
  override def  code_delta(i: Int, label: Label): Tree = {
    val target = dfa.delta(i, label);

    if (target == null)
      label match {
        case DefaultLabel() =>
          code_error(); // this may not happen !
        case _ =>
          null; // not good
      }
    else if (target.intValue() == dfa.nstates() - 1) // that one is a dead state
      code_fail();
    else
      callFun(scala.List( Literal(target.intValue() )));
  }

}
}


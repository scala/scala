package scala.util.grammar;

import scala.util.alphabet.Alphabet ;
import scala.collection.mutable ;

/** a mutable representation of hedge grammars. A hedge grammar over an
 *  alphabet consists of tree and hedge nonterminals (consecutive integers),
 *  and tree and hedge productions that relate them. Hedge nonterminals that
 *  can derive the empty hedge are called "nullable". initials tree
 *  or hedge nonterminals.
 */
class MutableTreeHedgeGrammar[ A <: Alphabet ] extends TreeHedgeGrammar[ A ] {

  /** number of tree nonterminals*/
  var nTreeNT: Int = 0;
  /** number of hedge nonterminals*/
  var nHedgeNT: Int = 0;
  /** inv: treeInitials.length == nTreeNT */
  val treeInitials    = new mutable.ResizableBitSet();
  /** inv: hedgeInitials.length == nHedgeNT */
  val hedgeInitials   = new mutable.ResizableBitSet();
  /** inv: hedgeIsNullable.length == nHedgeNT */
  val isNullable      = new mutable.ResizableBitSet();;
  val treeTransitions:  mutable.Map[Int, mutable.Set[TreeRHS]] =
    new mutable.HashMap[Int, mutable.Set[TreeRHS]];
  val hedgeTransitions: mutable.Map[Int, mutable.Set[HedgeRHS]] =
    new mutable.HashMap[Int, mutable.Set[HedgeRHS]];

  def makeTreeNT = {
    val r = nTreeNT;
    nTreeNT = nTreeNT + 1;
    treeInitials.ensureSize( nTreeNT );
    treeTransitions.update(r, new mutable.HashSet[TreeRHS]());
    r
  }

  def makeHedgeNT = {
    val r = nTreeNT;
    nHedgeNT = nHedgeNT + 1;
    hedgeInitials.ensureSize( nHedgeNT );
    hedgeTransitions.update(r, new mutable.HashSet[HedgeRHS]());
    r
  }

  def addHedgeRule(hnt1: Int,tnt: Int,hnt2: Int): Unit =
    addHedgeRule(hnt1, HedgeRHS(tnt, hnt2));

  def addHedgeRule(hnt: Int, rhs: HedgeRHS): Unit =
    hedgeTransitions( hnt ) += rhs;

  def addTreeRule(tnt: Int, label: A, hnt: Int): Unit =
    addTreeRule(tnt, TreeRHS( label, hnt ));

  def addTreeRule(tnt: Int,rhs: TreeRHS): Unit =
    treeTransitions( tnt ) += rhs;

}

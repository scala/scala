package scala.util.grammar;

import scala.collection.mutable ;

/** a mutable representation of hedge grammars. A hedge grammar over an
 *  alphabet consists of tree and hedge nonterminals (consecutive integers),
 *  and tree and hedge productions that relate them. Hedge nonterminals that
 *  can derive the empty hedge are called "nullable". initials tree
 *  or hedge nonterminals.
 */
class MutableTreeHedgeGrammar[ A ] extends TreeHedgeGrammar {

  /** number of tree nonterminals*/
  var nTreeNT: Int = 0;
  /** number of hedge nonterminals*/
  var nHedgeNT: Int = 0;
  /** inv: treeInitials.length == nTreeNT */
  val treeInitials    = new mutable.BitSet();
  /** inv: hedgeInitials.length == nHedgeNT */
  val hedgeInitials   = new mutable.BitSet();
  /** inv: hedgeIsNullable.length == nHedgeNT */
  val isNullable      = new mutable.BitSet();;
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

  def addConsRule(hnt1: Int,tnt: Int,hnt2: Int): Unit =
    addHedgeRule(hnt1, ConsRHS(tnt, hnt2));

  def addAnyHedgeRule(hnt: Int): Unit =
    addHedgeRule(hnt, AnyHedgeRHS);

  def addEmptyHedgeRule(hnt: Int): Unit =
    addHedgeRule(hnt, EmptyHedgeRHS);

  def addHedgeRule(hnt: Int, rhs: HedgeRHS): Unit =
    hedgeTransitions( hnt ) += rhs;

  def addTreeRule(tnt: Int, label: A, hnt: Int): Unit =
    addTreeRule(tnt, LabelledRHS( label, hnt ));

  def addAnyTreeRule(tnt: Int): Unit =
    addTreeRule(tnt, AnyTreeRHS);

  def addTreeRule(tnt: Int,rhs: TreeRHS): Unit =
    treeTransitions( tnt ) += rhs;

}

package scala.util.grammar;

import scala.collection.{ BitSet, Set, mutable } ;

/** a mutable representation of hedge grammars. A hedge grammar over an
 *  alphabet consists of tree and hedge nonterminals (consecutive integers),
 *  and tree and hedge productions that relate them. Hedge nonterminals that
 *  can derive the empty hedge are called "nullable". initials tree
 *  or hedge nonterminals.
 */
abstract class TreeHedgeGrammar {

  /** number of tree nonterminals*/
  def nTreeNT: Int;
  /** number of hedge nonterminals*/
  def nHedgeNT: Int;
  /** inv: treeInitials.size == nTreeNT */
  def  treeInitials: BitSet;
  /** inv: hedgeInitials.size == nHedgeNT */
  def hedgeInitials: BitSet;
  /** inv: isNullable.size == nHedgeNT */
  def isNullable: BitSet;

  def treeTransitions:  Array[Set[TreeRHS]] ;
  def hedgeTransitions: Array[Set[HedgeRHS]] ;

}

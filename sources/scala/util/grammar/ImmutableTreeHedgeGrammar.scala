package scala.util.grammar;

import scala.util.alphabet.Alphabet ;
import scala.collection.immutable ;

/** a mutable representation of hedge grammars. A hedge grammar over an
 *  alphabet consists of tree and hedge nonterminals (consecutive integers),
 *  and tree and hedge productions that relate them. Hedge nonterminals that
 *  can derive the empty hedge are called "nullable". initials tree
 *  or hedge nonterminals.
 */
abstract class ImmutableTreeHedgeGrammar[ A <: Alphabet ] extends TreeHedgeGrammar {

  /** number of tree nonterminals*/
  val nTreeNT: Int;
  /** number of hedge nonterminals*/
  val nHedgeNT: Int;
  /** inv: treeInitials.length == nTreeNT */
  val treeInitials: immutable.BitSet;
  /** inv: hedgeInitials.length == nHedgeNT */
  val hedgeInitials: immutable.BitSet;
  /** inv: isNullable.length == nHedgeNT */
  val isNullable: immutable.BitSet;
  var treeTransitions:  Array[immutable.Set[TreeRHS]];
  var hedgeTransitions: Array[immutable.Set[HedgeRHS]];

}

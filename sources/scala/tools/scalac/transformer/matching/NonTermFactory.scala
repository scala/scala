package scala.tools.scalac.transformer.matching;

import scala.runtime.matching.{TreeNT, HedgeNT};
import scala.collection.immutable;

/** factory for nonterminals. maintains list of initial nonterminals
 */
class NonTermFactory(treeInit: Int, hedgeInit: Int) {

  var treeInitials:immutable.Set[TreeNT] = new immutable.TreeSet[TreeNT]();
  var hedgeInitials:immutable.Set[HedgeNT] = new immutable.TreeSet[HedgeNT]();

  private var treeCounter = treeInit;
  private var hedgCounter = hedgeInit;

  def HedgeNT: HedgeNT = {
    val x = new HedgeNT( hedgCounter );
    hedgCounter = hedgCounter + 1;
    x
  }

  def TreeNT( vs:immutable.Set[Int] ): TreeNT = {
    val x = new TreeNT( treeCounter );
    x.vset = vs;
    treeCounter = treeCounter + 1;
    x
    }

  def initialHedgeNT = {
    val h = HedgeNT;
    hedgeInitials = hedgeInitials + h;
    h
  }

  def initialTreeNT = {
    val t = TreeNT( immutable.ListSet.Empty[Int] );
    treeInitials = treeInitials + t;
    t
  }

}

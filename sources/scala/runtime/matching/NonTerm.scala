package scala.runtime.matching ;

import scala.collection.immutable ;

abstract class NonTerm  {

  override def toString() = this match {
    case TreeNT( i ) => "t"+i;
    case h @ HedgeNT( i ) => "h"+i+{ if( h.nullable )"~" else "" };
  }

}

/** tree nonterminal. holds set of binding variable indices.
 */
case class TreeNT(i:int) extends NonTerm with Ordered[TreeNT] {

  var vset:immutable.Set[Int] = new immutable.TreeSet[Int]() ;

  /** vset should be sorted to allow fast lookup */
  def this( i:int, vset:immutable.Set[Int] ) = {
    this( i );
    this.vset = new immutable.TreeSet[Int]() incl vset ;
  }

  def compareTo [b >: TreeNT <% Ordered[b]](y: b): int = y match {
    case TreeNT( y1 ) => i compareTo y1;
    case _ => -(y compareTo this)
  }
};

/** hedge nonterminals. holds nullable property.
 */
case class HedgeNT(i:int) extends NonTerm with Ordered[HedgeNT] {

  var nullable:boolean = false;

  def this( i:int, nullable:boolean ) = {
    this( i );
    this.nullable = nullable;
  }

  def compareTo [b >: HedgeNT <% Ordered[b]](y: b): int = y match {
    case HedgeNT( y1 ) => i compareTo y1;
    case _ => -(y compareTo this)
  }
};

object EMPTYHEDGE extends HedgeNT( 0, true ) ;
object ANYHEDGE   extends HedgeNT( 1, true ) ;
object ANYTREE    extends TreeNT( 1 );

/** factory for nonterminals. maintains list of initial nonterminals
 */
class NonTermFactory {

  var treeInitials:immutable.Set[TreeNT] = new immutable.TreeSet[TreeNT]();
  var hedgeInitials:immutable.Set[HedgeNT] = new immutable.TreeSet[HedgeNT]();

  private var treeCounter = 2;
  private var hedgCounter = 2;

  def HedgeNT: HedgeNT = {
    val x = new HedgeNT( hedgCounter );
    hedgCounter = hedgCounter + 1;
    x
  }

  def TreeNT( vs:immutable.Set[Int] ): TreeNT = {
    val x = new TreeNT( treeCounter, vs );
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

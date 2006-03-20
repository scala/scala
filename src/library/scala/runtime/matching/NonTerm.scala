/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


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

  def compare [b >: TreeNT <% Ordered[b]](y: b): int = y match {
    case TreeNT( y1 ) => i compare y1;
    case _ => -(y compare this)
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

  def compare [b >: HedgeNT <% Ordered[b]](y: b): int = y match {
    case HedgeNT( y1 ) => i compare y1;
    case _ => -(y compare this)
  }
};

//case object EMPTYHEDGE extends HedgeNT( 0, true ) ;
//case object ANYHEDGE   extends HedgeNT( 1, true ) ;
object ANYTREE    extends TreeNT( 1 );


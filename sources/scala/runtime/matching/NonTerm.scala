package scala.runtime.matching ;

import scala.collection.immutable ;

abstract class NonTerm  {

  override def toString() = this match {
    case TreeNT( i ) => "t"+i;
    case HedgeNT( i ) => "h"+i;
  }
}

case class TreeNT(i:int) extends NonTerm with Ordered[TreeNT] {
  def compareTo [b >: TreeNT <% Ordered[b]](y: b): int = y match {
    case TreeNT( y1 ) => i compareTo y1;
    case _ => -(y compareTo this)
  }
  var vset:immutable.Set[Int] = immutable.ListSet.Empty ;
};
case class HedgeNT(i:int) extends NonTerm with Ordered[HedgeNT] {
  def compareTo [b >: HedgeNT <% Ordered[b]](y: b): int = y match {
    case HedgeNT( y1 ) => i compareTo y1;
    case _ => -(y compareTo this)
  }
  var nullable:boolean = false;
  //val vset:immutable.Set[Int] = immutable.ListSet.Empty ;
};

object EmptySeqNT extends HedgeNT(0) {
  override val nullable = true;
  };
object AnyHedgeNT extends HedgeNT(1); // for the "anytree-rule": (A,("",AnyTreeNT))
object ANYTREE extends TreeNT(1);
// for the "anynode-rule": (A,("",HedgeNT B))

class NonTermFactory {

  var treeInitials:immutable.Set[TreeNT] = new immutable.TreeSet[TreeNT]();
  var hedgeInitials:immutable.Set[HedgeNT] = new immutable.TreeSet[HedgeNT]();

  //private var counter = 4;
  private var treeCounter = 2;
  private var hedgCounter = 2;

  def HedgeNT: HedgeNT = {
    val x = new HedgeNT( hedgCounter );
    hedgCounter = hedgCounter + 1;
    x
  }

  def TreeNT( vs:immutable.Set[Int] ): TreeNT = {
    /*
    val x = new TreeNT(  treeCounter ) {
      override var vset = vs;
    };
    */
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

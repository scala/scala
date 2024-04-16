//> using options -Werror -Xlint:deprecation
//

import scala.language.{ implicitConversions }
abstract class BaseList {
  type Node <: BaseNodeImpl
  implicit def convertNode(ni : BaseNodeImpl): Node = ni.asInstanceOf[Node];
  abstract class BaseNodeImpl
}
abstract class LinkedList extends BaseList {
  type Node <: NodeImpl0
  trait NodeImpl0 extends super.BaseNodeImpl;
}
trait OffsetList extends LinkedList {
  type Node <: NodeImpl1
  trait NodeImpl1 extends super.NodeImpl0
}

trait PriorityTree extends BaseList {
  type Node <: NodeImpl2
  trait NodeImpl2 extends super.BaseNodeImpl {
    def chop : Node = this
  }
}

trait PrecedenceParser  extends LinkedList with PriorityTree {
  type Node <: NodeImpl3
  trait NodeImpl3 extends super[LinkedList].NodeImpl0 with super[PriorityTree].NodeImpl2
}

trait Matcher extends PrecedenceParser {
  type Node <: NodeImpl4
  trait NodeImpl4 extends super.NodeImpl3

  type Matchable <: Node with MatchableImpl0
  implicit def convertMatchable(m : MatchableImpl0): Matchable = m.asInstanceOf[Matchable]
  trait MatchableImpl0 extends NodeImpl4 {
    override def chop : Node = {
      Console.println("passed"); super.chop;
    }
  }
}

class Test1 extends OffsetList with Matcher {
  type Node = NodeImpl5
  trait NodeImpl5 extends super[OffsetList].NodeImpl1 with super[Matcher].NodeImpl4
  class MatchableImpl1 extends super.MatchableImpl0 with NodeImpl5
  type Matchable = MatchableImpl1
}

object Test extends App {
  val test = new Test1
  val m = new test.MatchableImpl1
  m.chop
}

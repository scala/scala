
import scala.language.{ implicitConversions }
abstract class BaseList {
  type Node <: NodeImpl;
  implicit def convertNode(ni : NodeImpl) = ni.asInstanceOf[Node];
  abstract class NodeImpl;
}
abstract class LinkedList extends BaseList {
  type Node <: NodeImpl;
  trait NodeImpl extends super.NodeImpl;
}
trait OffsetList extends LinkedList {
  type Node <: NodeImpl;
  trait NodeImpl extends super.NodeImpl;
}

trait PriorityTree extends BaseList {
  type Node <: NodeImpl;
  trait NodeImpl extends super.NodeImpl {
    def chop : Node = this;
  }
}

trait PrecedenceParser  extends LinkedList with PriorityTree {
  type Node <: NodeImpl;
  trait NodeImpl extends super[LinkedList].NodeImpl with super[PriorityTree].NodeImpl;
}

trait Matcher extends PrecedenceParser {
  type Node <: NodeImpl;
  trait NodeImpl extends super.NodeImpl;

  type Matchable <: Node with MatchableImpl;
  implicit def convertMatchable(m : MatchableImpl) = m.asInstanceOf[Matchable];
  trait MatchableImpl extends NodeImpl {
    override def chop : Node = {
      Console.println("passed"); super.chop;
    }
  }
}

class Test1 extends OffsetList with Matcher {
  type Node = NodeImpl;
  trait NodeImpl extends super[OffsetList].NodeImpl with super[Matcher].NodeImpl;
  class MatchableImpl extends super.MatchableImpl with NodeImpl;
  type Matchable = MatchableImpl;
}

object Test extends App {
  val test = new Test1;
  val m = new test.MatchableImpl;
  m.chop;
}

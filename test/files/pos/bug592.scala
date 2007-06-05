trait Graph {
  type Edge;
  type Node <: NodeIntf;

  trait NodeIntf {
    def connectWith(node: Node): Edge;
  }

  def nodes: List[Node];
  def edges: List[Edge];
  def addNode: Node;

  protected var ids = 0;
}

abstract class DirectedGraph extends Graph {
  type Edge <: EdgeImpl;

  class EdgeImpl(origin: Node, dest: Node) {
    def from = origin;
    def to = dest;
    override def toString = ""+origin+" --> "+dest
  }

  class NodeImpl extends NodeIntf { self: Node =>
    val id = ids
    ids = ids + 1
    def connectWith(node: Node): Edge = {
      val edge = newEdge(this, node);
      edges = edge :: edges;
      edge;
    }
    override def toString = "Node "+id
  }

  protected def newNode: Node;
  protected def newEdge(from: Node, to: Node): Edge;
  var nodes: List[Node] = Nil;
  var edges: List[Edge] = Nil;

  def addNode: Node = {
    val node = newNode;
    nodes = node :: nodes;
    node;
  }
}

class ConcreteDirectedGraph extends DirectedGraph {
  type Edge = EdgeImpl;
  type Node = NodeImpl;

  protected def newNode: Node = {
    new NodeImpl;
  }

  protected def newEdge(f: Node, t: Node): Edge = {
    new EdgeImpl(f, t);
  }
}

object ExplicitThis {
  def main(args: Array[String]): Unit = {
    val g: Graph = new ConcreteDirectedGraph;
    val n1 = g.addNode;
    val n2 = g.addNode;
    val n3 = g.addNode;
    Console.println(n1.connectWith(n2))
    Console.println(n2.connectWith(n3))
    Console.println(n1.connectWith(n3))
  }
}

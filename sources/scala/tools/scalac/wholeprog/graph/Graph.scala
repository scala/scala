/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scala.collection.mutable._;

package scala.tools.scalac.wholeprog.graph {


/** Defualt implementation for Node objects, which can have any
  * identifier */
class Node[id_t, info_t](i: id_t, nn: info_t) {
  var info: info_t = nn;
  val id: id_t = i;

  override def toString(): String = "\"" + id.hashCode() +
    "\"[ label = \"" + info + "\" ]";
}

/** Default implementation for edges. It takes two parameters, the
  * start and the end identifiers for the nodes it connects  */
class Edge[id_t, info_t](start: id_t, end: id_t) {
  val from = start;
  val to = end;
  var info: info_t = _;

  override def toString(): String =  "\"" + start.hashCode() + "\" -> " + "\""
      + end.hashCode() + "\"[ label = \"" + info + "\" ]";
}


class InvalidEdgeException(from: String, to: String) extends java.lang.Exception {
  override def toString(): String = "Edge [" + from + " -> " + to +
      "] references non existent nodes";
}


/** The Graph class, parameterized with the node and edge types
  * The id_t is the type of the identifier of nodes. This is used
  * when retrieving nodes from the graph. The node_t is the type of
    nodes, which have to subtype the Node trait. edge_t is the type
    of edges, which again is a subtype of Edge.   */

class Graph[id_t, node_info_t, edge_info_t] {
  type node_t = Node[id_t, node_info_t];
  type edge_t = Edge[id_t, edge_info_t];

  var nodes: HashMap[id_t, node_t] = new HashMap[id_t, node_t];
  var edges: List[edge_t] = Nil;

  val inEdges: HashMap[id_t, List[edge_t]] = new HashMap[id_t, List[edge_t]];
  val outEdges: HashMap[id_t, List[edge_t]] = new HashMap[id_t, List[edge_t]];


  def addNode(n: node_t): Unit = {
    nodes += n.id -> n;
    inEdges += n.id -> Nil;
    outEdges += n.id -> Nil;
  }

  def addEdge(from: id_t, to: id_t): edge_t = addEdge(new Edge(from, to));

  def addEdge(e: edge_t): edge_t = {
    if ((nodes contains e.from) && (nodes contains e.to)) {
      edges = e :: edges;
      addEdgeToIncidenceMap(outEdges, e, getNode(e.from));
      addEdgeToIncidenceMap(inEdges, e, getNode(e.to));
    } //else
//      throw new InvalidEdgeException(e.start, e.end);
    e
  }

  def removeEdge(e: edge_t): edge_t = {
    edges = edges.remove( ee => e == ee );
    outEdges += e.from -> getOutEdges(e.from).remove( ee => e == ee);
    inEdges  += e.to -> getInEdges(e.to).remove(ee => e == ee);

    e
  }

  def removeEdges(es: List[edge_t]): Unit = es match {
    case Nil => ();
    case e :: ee => { removeEdge(e); removeEdges(ee) }
  }

  def addEdgeToIncidenceMap(map: HashMap[id_t, List[edge_t]], e: edge_t, n: node_t): Unit = {
    map.get(n.id) match {
      case Some(l) => map += n.id -> (e :: l);
      case None    => map += n.id -> (e :: Nil);
    }
  }

  def getOutEdges(node: id_t): List[edge_t] = outEdges.get(node) match {
    case Some(l) => l;
    case None    => Nil;
  }

  def getInEdges(node: id_t): List[edge_t] = inEdges.get(node) match {
    case Some(l) => l;
    case None    => Nil;
  }

  def visitDFS(f: (node_t) => Unit): Unit = {
    val visited = new HashSet[node_t];
    var i = nodeIterator;

    while (i.hasNext) {
      visitDFS(i.next, f, visited);
    }
  }

  private def visitDFS(currentNode: node_t, f: (node_t) => Unit, visited: Set[node_t]): Unit = {
    if (!visited.contains(currentNode)) {
      visited += currentNode;
      f(currentNode);

      getOutEdges(currentNode.id) foreach { (e) => {
	visitDFS(getNode(e.to), f, visited);
      }}
    }
  }

  def getNode(id: id_t): node_t = nodes.get(id) match {
    case Some(n) => n;
    case None    => null;
  }

  def getRootNodes: Iterator[node_t] = {
    for (val elem <- nodes.elements; getInEdges(elem._2.id) == Nil)
	yield elem._2;
  }

  /** Remove all nodes with degree 0. Return the graph. */
  def prune: Graph[id_t, node_info_t, edge_info_t] = {
    nodes.filter( (id, n) => inEdges(id) != Nil || outEdges(id) != Nil );

    this
  }

  /* graph visualization  */

  override def toString(): String = {
    var s = "";

    edges.foreach( (e) => s = s + e + "\n" );
    s
  }

  def toDotString: String = {
    var str = "digraph grph {\n";

    val it = nodeIterator;
    while (it.hasNext) {
      val node = it.next;
      str = str + node + "\n";// + " [label = \"" + node.id + "\"];\n";
    }

    // edges
    edges.foreach( (e) => str = str + e + "\n" );
    str + "}\n";
  }

  def nodeIterator: GraphIterator = new GraphIterator();

  /** Iterator for graph nodes */
  class GraphIterator extends Iterator[node_t] {
    val elem = nodes.elements;

    override def hasNext: Boolean = elem.hasNext;
    override def next: node_t = elem.next._2;
  }


  def consistencyCheck: Unit = {
    edges.foreach( (e) => {
      if ( !(getOutEdges(e.from) contains e) )
	Console.println("Edge " + e + " not found in out edges of " + e.from);

      if ( !(getInEdges(e.to) contains e) )
	Console.println("Edge " + e + " not found in in edges of " + e.to);
    });
  }
}

} // graph package

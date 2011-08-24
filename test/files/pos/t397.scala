abstract class Root {

  abstract class Edge {
    type V;
    def source: V;
  }

  abstract class Graph {
    type W;
    type E <: Edge{type V = W};
    def edge: E;
  }

  val g: Graph{type W = Int};
  val x: Int = g.edge.source;
}

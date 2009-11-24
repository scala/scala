package graphs;

abstract class Graph (private[graphs] val mappings : Any){
}

class Nodes (mappings : Any)  extends Graph(mappings) {
  mappings.toString;
}

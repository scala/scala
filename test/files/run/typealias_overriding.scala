// this bug (http://scala-webapps.epfl.ch/bugtracking/bugs/displayItem.do?id=1065)
// was caused by Uncurry not normalizing all the types 
// (more specifically the argument/return types of an anonymous Function)
object Test extends App {
  trait AddRemove {
    type TNode <: NodeImpl;
    trait NodeImpl;
    
    object removing {
      type TNode = AddRemove.this.TNode;
      def printNode(node: TNode, f: TNode => String) = Console.println(f(node))
    }
  }
  
  class Linked extends AddRemove {
    type TNode = Node // can also directly write `class Node extends super.NodeImpl' -- doesn't change the bug
    class Node extends super.NodeImpl { override def toString = "LinkedNode" }
    
    removing.printNode(new Node, (x: removing.TNode) => x.toString) // make inference explicit, doesn't affect the bug
  }

  new Linked
}

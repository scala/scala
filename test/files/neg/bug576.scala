package lampion.collections;

abstract class BaseListXXX {
  type Node <: BaseNode;
  abstract class BaseNode {
  }
}  
trait PriorityTreeXXX extends BaseListXXX {
	type Node <: BasicTreeNode;
  
  trait BasicTreeNode extends BaseNode {
    def sibling: Node; 
    def insert(dir : Int, node : Node) = {
      if (true) sibling.insert(node);
      //else insert(node);
      
    }
    def insert(node : Node) : Unit  = {}
  }
}

trait PriorityTree {
  type Node <: BasicTreeNode;
  
  val top = initTree;
  top.next = (initTree);
  top.next.prev = (top);
  
  def initTree : Node;
  
  

  
  trait BasicTreeNode {
    private[PriorityTree] var next  : Node = _;
    private[PriorityTree] var prev  : Node = _;
    private[PriorityTree] var chld : Node = _;
    //var next  : Node = _;
    //var prev  : Node = _;
    //var chld : Node = _;
  }
}

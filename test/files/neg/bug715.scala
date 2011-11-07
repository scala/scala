package test; 
trait B {
  type Node <: NodeImpl;
  trait NodeImpl {
    def self : Node;
    def chilren : List[Node];
  }
}
trait C extends B {
  type Node <: NodeImpl;
  trait NodeImpl extends super.NodeImpl {
    override def children = super.chilren;
    children;
  }
}

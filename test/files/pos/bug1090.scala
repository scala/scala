object Test {
  trait Manager {
    type Node;
    def iterator : Iterator[Node]
  }
  trait Core {
    type Node;
    trait NodeImpl
    trait Manager extends Test.Manager {
      type Node = Core.this.Node
    }
    def f(manager : Manager) = manager.iterator.foreach{
      case node : NodeImpl => 
    }
  }
}

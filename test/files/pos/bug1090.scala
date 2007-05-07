object Test {
  trait Manager {
    type Node;
    def elements : Iterator[Node]
  }
  trait Core {
    type Node;
    trait NodeImpl
    trait Manager extends Test.Manager {
      type Node = Core.this.Node
    }
    def f(manager : Manager) = manager.elements.foreach{
      case node : NodeImpl =>
    }
  }
}
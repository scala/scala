trait SelfTyped {
  type Self
}
 
sealed trait Container { self =>
  type Child = SelfTyped { type Self <: self.ChildSelf }
  type ChildSelf
  def child: Child
}
 
sealed trait ExampleChild extends SelfTyped
 
case class Example (child: Example#Child) extends Container {
  type Self = Example
  type ChildSelf = ExampleChild
}
 
sealed trait AbstractNode extends ExampleChild { }
sealed case class ConcreteNode () extends AbstractNode { override type Self = ConcreteNode }
 
object ProofOfConcept {
  def processContainer(container: Container): Unit = {
    val c: container.Child = container.
    c match {
      case n: ConcreteNode => // java.lang.ClassCastException: ConcreteNode cannot be cast to scala.runtime.Null$
        n
    }
  }
}
object Test extends App {
  ProofOfConcept.processContainer(Example(ConcreteNode()))
}

abstract class Node extends NodeSeq
trait NodeSeq extends Seq[Node]
object NodeSeq {
  implicit def seqToNodeSeq(ns: Seq[Node]): NodeSeq = ???
   def foo[B, That](f: Seq[B])(implicit bf: scala.collection.generic.CanBuildFrom[Seq[Int], B, That]): That = ???
}

class Transformer {
  def apply(nodes: Any): Any = ???
}

object transformer1 extends Transformer {
  // Adding explicit type arguments, or making the implicit view
  // seqToNodeSeq explicit avoids the crash
  NodeSeq.foo {
    // These both avoid the crash:
    // val t = new Transformer {}; t.apply(null)
    // new Transformer().apply(null)
    new Transformer {}.apply(null)

    null: NodeSeq
  }: NodeSeq
}


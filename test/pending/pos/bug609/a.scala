package test;
abstract class MyLinkedList  {
  type Node <: NodeImpl;
  def init : Node;
  final class Segment(node : Node);
  trait NodeImpl {
    def insertAfter(node : Node) : Unit = insertAfter(new Segment(node));
    def insertAfter(segment : Segment) = {}
  }
}
trait MyOffsetList extends MyLinkedList {
  type Node <: NodeImpl;
  trait NodeImpl  extends super.NodeImpl {
    override def insertAfter(segment : Segment) = super.insertAfter(segment);
  }
}

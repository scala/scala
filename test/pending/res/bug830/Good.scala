package test;
trait HasNodeXXX {
  type Node <: NodeImpl;
  trait NodeImpl;
}
trait ScalaFlowScannerZZZ extends HasNodeXXX {
  type Node <: NodeImpl;
  trait NodeImplA extends super.NodeImpl;
  trait NodeImplB extends super.NodeImpl;
  trait NodeImpl extends NodeImplA with NodeImplB;
}

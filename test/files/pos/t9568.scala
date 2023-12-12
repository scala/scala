object Cyclic {
  class Node[T]() {
    type Self = T
  }

  val nodeA = new Node[Int]()
  val nodeB = new NodeB(a => a)
  val nodeC = new NodeC(a => a)
  /*
  val nodeB = new NodeB(a => a + 1)
  val nodeC = new NodeC(a => a + 1)
  */
  val nodeD = new NodeD((b, c) => b + c)

  class NodeB[T](fun: Function[nodeA.Self, T]) extends Node[T]

  class NodeC[T](fun: Function[nodeA.Self, T]) extends Node[T]

  class NodeD[T](fun: Function2[nodeB.Self, nodeC.Self, T]) extends Node[T]
}

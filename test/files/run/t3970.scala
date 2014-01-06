


import collection.mutable._



object Test {
  def main(args: Array[String]) {
    val dl = DoubleLinkedList[Int]()
    dl.remove()

    val dl2 = DoubleLinkedList[Int](1, 2, 3)
    dl2.next.remove()
    assert(dl2 == DoubleLinkedList(1, 3))

    val dl3 = DoubleLinkedList[Int](1, 2, 3)
    assert(dl3.drop(1) == DoubleLinkedList(2, 3))
    assert(dl3.drop(1).prev == null)
  }
}

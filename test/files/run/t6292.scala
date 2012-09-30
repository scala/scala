 import scala.collection.mutable.DoubleLinkedList

object Test {
  def main(args: Array[String]): Unit = {
    cloneAndtest(DoubleLinkedList[Int]())
    cloneAndtest(DoubleLinkedList[Int](1))
    cloneAndtest(DoubleLinkedList[Int](1,2,3,4))
  }

  def cloneAndtest(l: DoubleLinkedList[Int]): Unit =
    testSame(l, l.clone.asInstanceOf[DoubleLinkedList[Int]])

  def testSame(one: DoubleLinkedList[Int], two: DoubleLinkedList[Int]): Unit = {
    def msg = s" for ${one} and ${two} !"
    assert(one.size == two.size, s"Cloned sizes are not the same $msg!")
    assert(one == two, s"Cloned lists are not equal $msg")
  }
}

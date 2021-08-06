package scala.collection.mutable

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class SetTest {

  class MySet(self: Set[String]) extends Set[String] with SetOps[String, Set, MySet] {
    override protected[this] def fromSpecific(coll: IterableOnce[String]): MySet = new MySet(iterableFactory.from(coll))
    override protected[this] def newSpecificBuilder: Builder[String, MySet] = iterableFactory.newBuilder[String].mapResult(new MySet(_))

    def subtractOne(elem: String) = { self -= elem; this }
    def addOne(elem: String) = { self += elem; this }

    override def empty = new MySet(self.empty)
    def iterator = self.iterator
    def contains(elem: String) = self.contains(elem)
    def clear(): Unit = self.clear()
  }

  @Test
  def hasCorrectClear(): Unit = {
    val s = new MySet(Set("EXPOSEDNODE", "CONNECTABLE"))
    s.clear()
    assertEquals(new MySet(Set()), s)
  }
}

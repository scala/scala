package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SetLikeTest {

  class MySet(self: Set[String]) extends Set[String] with SetLike[String, MySet] {
    override def -=(elem: String) = { self -= elem; this }
    override def +=(elem: String) = { self += elem; this }

    override def empty = new MySet(self.empty)
    override def iterator = self.iterator
    override def contains(elem: String) = self.contains(elem)
  }

  @Test
  def hasCorrectClear() {
    val s = new MySet(Set("EXPOSEDNODE", "CONNECTABLE"))
    s.clear()
    assertEquals(new MySet(Set()), s)
  }
}

package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.IterableFactory

@RunWith(classOf[JUnit4])
class SetTest {

  class MySet(self: Set[String]) extends Set[String] with SetOps[String, Set, MySet] {
    override protected[this] def fromSpecificIterable(coll: scala.collection.Iterable[String]): MySet = new MySet(fromIterable(coll))
    override protected[this] def newSpecificBuilder: Builder[String, MySet] = iterableFactory.newBuilder[String].mapResult(new MySet(_))

    def subtractOne(elem: String) = { self -= elem; this }
    def addOne(elem: String) = { self += elem; this }

    override def empty = new MySet(self.empty)
    def iterator = self.iterator
    def contains[A1 >: String](elem: A1) = self.contains(elem)
    def get(elem: String): Option[String] = self.get(elem)
    def clear(): Unit = self.clear()
  }

  @Test
  def hasCorrectClear(): Unit = {
    val s = new MySet(Set("EXPOSEDNODE", "CONNECTABLE"))
    s.clear()
    assertEquals(new MySet(Set()), s)
  }

  @Test
  def flatMapInPlace(): Unit = {
    val s = Set(1, 2)
    val tens = s.flatMapInPlace(e => Set(e, e + 10))
    assert(tens == Set(1, 11, 2, 12))
  }

  @Test
  def flatMapInPlaceAddOne(): Unit = {
    val s = Set(1, 2, 3)
    val ss = s.flatMapInPlace(e => Set(e + 1, e + 2))
    assert(ss == Set(2, 3, 4, 5))
  }

  @Test // From strawman collection #509
  def flatMapInPlace_dropElements(): Unit = {
    val set = Set(1)
    val empty = Set.empty[Int]
    val fmip = set.flatMapInPlace(_ => empty)
    assert(fmip.size == 0)
  }
}

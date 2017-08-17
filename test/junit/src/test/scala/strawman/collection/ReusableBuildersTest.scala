package strawman.collection

import strawman.collection.immutable.{List, Nil, Vector}
import scala.Predef.{genericArrayOps => _, classOf, assert, intWrapper}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

/* Tests various maps by making sure they all agree on the same answers. */
@RunWith(classOf[JUnit4])
class ReusableBuildersTest {
  // GrowingBuilders are NOT reusable but can clear themselves
  @Test
  def test_SI8648(): Unit = {
    val b = mutable.HashSet.newBuilder[Int]()
    b += 3
    b.clear()
    assert(!b.isInstanceOf[mutable.ReusableBuilder[_,_]])
    assert(b.isInstanceOf[mutable.GrowableBuilder[_,_]])
    assert(b.result == Set[Int]())
  }

  // ArrayBuilders ARE reusable, regardless of whether they returned their internal array or not
  @Test
  def test_SI9564(): Unit = {
    val b = Array.newBuilder[Float]
    b += 3f
    val three = b.result
    b.clear
    b ++= (1 to 16).map(_.toFloat)
    val sixteen = b.result
    b.clear
    b += 0f
    val zero = b.result
//    assert(b.isInstanceOf[mutable.ReusableBuilder[_,_]])
    assert(three.to(List) == 3 :: Nil)
    assert(sixteen.to(List) == immutable.Range.inclusive(1, 16))
    assert(zero.to(List) == 0 :: Nil)
  }

  @Test
  def test_reusability(): Unit = {
    val bl = List.newBuilder[String]()
    val bv = Vector.newBuilder[String]()
    val ba = mutable.ArrayBuffer.newBuilder[String]()
    assert(bl.isInstanceOf[mutable.ReusableBuilder[_, _]])
    assert(bv.isInstanceOf[mutable.ReusableBuilder[_, _]])
    assert(!ba.isInstanceOf[mutable.ReusableBuilder[_, _]])
  }
}

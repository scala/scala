package scala.collection.mutable

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.collection.immutable

@RunWith(classOf[JUnit4])
class WrappedArrayTest {
  @Test
  def ofRefEquality(): Unit = {
    def assertOfRef(left: Array[AnyRef], right: Array[AnyRef]): Unit = {
      assert(new WrappedArray.ofRef(left) == new WrappedArray.ofRef(right))
    }
    assertOfRef(Array(Int.box(65)), Array(Double.box(65.0)))
    assertOfRef(Array(Double.box(65.0)), Array(Int.box(65)))
    assertOfRef(Array(Int.box(65)), Array(Char.box('A')))
    assertOfRef(Array(Char.box('A')), Array(Int.box(65)))
  }

  @Test
  def byteArrayHashCodeEquality(): Unit = {
    val x = immutable.Seq[Byte](10)
    val y = Array[Byte](10).toSeq
    assertEquals(x, y)
    assertEquals(x.hashCode(), y.hashCode())
  }
}

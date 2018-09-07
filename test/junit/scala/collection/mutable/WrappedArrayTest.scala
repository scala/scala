package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

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
}

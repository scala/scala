package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.collection.mutable
import scala.reflect.ClassTag

@RunWith(classOf[JUnit4])
class WrappedArrayBuilderTest {
  @Test
  def reusable() {
    val builder = new WrappedArrayBuilder(ClassTag.Int)
    val vector = Vector.range(1, 17)
    val expected = new WrappedArray.ofInt(Vector.range(1, 17).toArray)

    builder ++= vector
    val actual = builder.result()
    assert( actual == expected )

    builder.clear()
    val expected2 = new WrappedArray.ofInt(Array[Int](100))
    builder += 100

    // Previously created WrappedArray MUST be immutable even after `result`, `clear` and some operation are called
    assert( actual == expected )
    assert( builder.result() == expected2 )
  }
}

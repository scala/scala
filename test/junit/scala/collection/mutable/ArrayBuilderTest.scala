package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.mutable

@RunWith(classOf[JUnit4])
class ArrayBuilderTest {
  @Test
  def reusable() {
    val builder = new ArrayBuilder.ofInt
    val vector = Vector.range(1, 17)
    val expected = Vector.range(1, 17).toArray

    builder ++= vector
    val actual = builder.result()
    assert ( actual.deep == expected.deep )

    builder.clear()
    val expected2 = Array[Int](100)
    builder += 100

    // Previously created array MUST be immutable even after `result`, `clear` and some operation are called
    assert( actual.deep == expected.deep )
    assert( builder.result().deep == expected2.deep )
  }
}

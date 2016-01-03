package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.tools.testing.AssertUtil

@RunWith(classOf[JUnit4])
class ArrayBuilderTest {

  @Test
  def reusingAfterClearIsSafe: Unit = {
    // Test for SI-9564:
    //Reusing ArrayBuilder after clear() overwrites its previous result

    def buildClearReuse[T](
        builder: ArrayBuilder[T],
        elements: scala.collection.immutable.Iterable[T],
        newElement: T): Unit = {

      builder ++= elements
      val array = builder.result

      builder.clear()

      builder += newElement // reuse the builder

      AssertUtil.assertSameElements(elements, array) 
    }

    val sixteen = 1 to 16

    buildClearReuse(new ArrayBuilder.ofByte, sixteen.map(_.toByte), 99.toByte)
    buildClearReuse(new ArrayBuilder.ofChar, sixteen.map(c => ('a' + c - 1).toChar), 'X')
    buildClearReuse(new ArrayBuilder.ofShort, sixteen.map(_.toShort), 99.toShort)
    buildClearReuse(new ArrayBuilder.ofInt, sixteen, 99)
    buildClearReuse(new ArrayBuilder.ofLong, sixteen.map(_.toLong), 99L)
    buildClearReuse(new ArrayBuilder.ofFloat, sixteen.map(_.toFloat), 99f)
    buildClearReuse(new ArrayBuilder.ofDouble, sixteen.map(_.toDouble), 99d)
    buildClearReuse(new ArrayBuilder.ofBoolean, sixteen.map(_ => true), false)
    buildClearReuse(new ArrayBuilder.ofRef[String], sixteen.map(_.toString), "X")
  }
}

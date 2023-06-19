package scala.collection.mutable

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import java.lang.reflect.InvocationTargetException
import scala.annotation.nowarn
import scala.runtime.PStatics
import scala.tools.testkit.AssertUtil.{assertSameElements, assertThrows, fail}
import scala.tools.testkit.ReflectUtil.{getMethodAccessible, _}
import scala.util.chaining._

class ArrayBuilderTest {
  @Test
  def testArrayBuilderOverflow(): Unit = {

    val ab:ArrayBuffer[Int] = new ArrayBuffer[Int]()

    var i:Int = 0
    while (i < PStatics.VM_MaxArraySize) {
      ab.addOne(i)
      i += 1
    }

    assertEquals(PStatics.VM_MaxArraySize, ab.size)
  }
}

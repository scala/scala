package scala.collection

import org.junit.Test
import org.junit.Assert._

import scala.collection.mutable.Buffer
import scala.reflect.ClassTag

// based on run/collections-conversion.scala partest
@deprecated("Tests conversion to Stream", since="2.13")
class CollectionConversionsTest {
  val out = new StringBuilder

  val testVector = Vector(1,2,3)
  val testBuffer = Buffer(1,2,3)
  val testSeq = Seq(1,2,3)
  val testStream = Stream(1,2,3)
  val testArray = Array(1,2,3)

  @Test def testAll(): Unit = {
    testConversion("iterator", () => (1 to 3).iterator)
    testConversion("Vector", () => Vector(1,2,3))
    testConversion("List", () => List(1,2,3))
    testConversion("Buffer", () => Buffer(1,2,3))
    testConversion("Set", () => Set(1,2,3))
    testConversion("SetView", () => Set(1,2,3).view)
    testConversion("BufferView", () => Buffer(1,2,3).view)
  }

  def printResult[A,B](msg: String, obj: A, expected: B)(implicit tag: ClassTag[A], tag2: ClassTag[B]): Boolean = {
    out ++= ("  :" + msg +": ")
    val isArray = obj match {
      case x: Array[Int] => true
      case _ => false
    }
    val expectedEquals =
      if(isArray) obj.asInstanceOf[Array[Int]].toSeq == expected.asInstanceOf[Array[Int]].toSeq
      else obj == expected
    val tagEquals = tag == tag2
    val ok = expectedEquals && tagEquals
    if(ok) out ++= "OK"
    else out ++= "FAILED"
    if(!expectedEquals) out ++= (", " + obj + " != " + expected)
    if(!tagEquals)     out ++= (", " + tag + " != " + tag2)
    out += '\n'
    ok
  }

  def testConversion[A: ClassTag](name: String, col: () => IterableOnce[A]): Unit = {
    out ++= ("-- Testing " + name + " ---\n")
    if(!(
      printResult("[Direct] Vector   ", col().iterator.toVector, testVector) &&
      printResult("[Copy]   Vector   ", col().iterator.to(Vector), testVector) &&
      printResult("[Direct] Buffer   ", col().iterator.toBuffer, testBuffer) &&
      printResult("[Copy]   Buffer   ", col().iterator.to(Buffer), testBuffer) &&
      printResult("[Copy]   Seq      ", col().iterator.to(Seq), testSeq) &&
      printResult("[Direct] Stream   ", col().iterator.toStream, testStream) &&
      printResult("[Copy]   Stream   ", col().iterator.to(Stream), testStream) &&
      printResult("[Direct] Array    ", col().iterator.toArray, testArray) &&
      printResult("[Copy]   Array    ", col().iterator.to(Array), testArray)
    )) {
      print(out)
      fail("Not all tests successful")
    }
  }

  @Test
  def t11976(): Unit = {
    import scala.jdk.CollectionConverters._
    val myMap_1 = java.util.Collections.singletonMap("a", 1)
    var x = 0
    myMap_1.asScala.partition { case (key, value) =>
      x += 1
      true
    }
    assertEquals(1, x)
  }
}


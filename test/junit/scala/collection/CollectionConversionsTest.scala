package scala.collection

import org.junit.Test
import org.junit.Assert._

import scala.collection.mutable.Buffer
import scala.reflect.ClassTag

// based on run/collections-conversion.scala partest
class CollectionConversionsTest {
  val out = new StringBuilder

  val testVector = Vector(1,2,3)
  val testBuffer = Buffer(1,2,3)
  val testGenSeq = GenSeq(1,2,3)
  val testSeq = Seq(1,2,3)
  val testStream = Stream(1,2,3)
  val testArray = Array(1,2,3)

  @Test def testAll: Unit = {
    testConversion("iterator", (1 to 3).iterator)
    testConversion("Vector", Vector(1,2,3))
    testConversion("List", List(1,2,3))
    testConversion("Buffer", Buffer(1,2,3))
    testConversion("Set", Set(1,2,3))
    testConversion("SetView", Set(1,2,3).view)
    testConversion("BufferView", Buffer(1,2,3).view)
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

  def testConversion[A: ClassTag](name: String, col: => GenTraversableOnce[A]): Unit = {
    val tmp = col
    out ++= ("-- Testing " + name + " ---\n")
    if(!(
      printResult("[Direct] Vector   ", col.toVector, testVector) &&
      printResult("[Copy]   Vector   ", col.to[Vector], testVector) &&
      printResult("[Direct] Buffer   ", col.toBuffer, testBuffer) &&
      printResult("[Copy]   Buffer   ", col.to[Buffer], testBuffer) &&
      printResult("[Direct] GenSeq   ", col.toSeq, testGenSeq) &&
      printResult("[Copy]   GenSeq   ", col.to[GenSeq], testGenSeq) &&
      printResult("[Copy]   Seq      ", col.to[Seq], testSeq) &&
      printResult("[Direct] Stream   ", col.toStream, testStream) &&
      printResult("[Copy]   Stream   ", col.to[Stream], testStream) &&
      printResult("[Direct] Array    ", col.toArray, testArray) &&
      printResult("[Copy]   Array    ", col.to[Array], testArray)
    )) {
      print(out)
      fail("Not all tests successful")
    }
  }
}


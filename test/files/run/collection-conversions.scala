import collection._
import mutable.Buffer
import parallel.immutable.ParVector
import parallel.mutable.ParArray
import reflect.ClassTag

object Test {

  def printResult[A,B](msg: String, obj: A, expected: B)(implicit tag: ClassTag[A], tag2: ClassTag[B]) = {
    print("  :" + msg +": ")
    val isArray = obj match {
      case x: Array[Int] => true
      case _ => false
    }
    val expectedEquals =
      if(isArray) obj.asInstanceOf[Array[Int]].toSeq == expected.asInstanceOf[Array[Int]].toSeq
      else obj == expected
    val tagEquals = tag == tag2
    if(expectedEquals && tagEquals) print("OK")
    else print("FAILED")
    if(!expectedEquals) print(", " + obj + " != " + expected)
    if(!tagEquals)     print(", " + tag + " != " + tag2)
    println("")
  }

  val testVector = Vector(1,2,3)
  val testBuffer = Buffer(1,2,3)
  val testGenSeq = GenSeq(1,2,3)
  val testSeq = Seq(1,2,3)
  val testStream = Stream(1,2,3)
  val testArray = Array(1,2,3)
  val testParVector = ParVector(1,2,3)
  val testParArray = ParArray(1,2,3)

  def testConversion[A: ClassTag](name: String, col: => GenTraversableOnce[A]): Unit = {
    val tmp = col
    println("-- Testing " + name + " ---")
    printResult("[Direct] Vector   ", col.toVector, testVector)
    printResult("[Copy]   Vector   ", col.to[Vector], testVector)
    printResult("[Direct] Buffer   ", col.toBuffer, testBuffer)
    printResult("[Copy]   Buffer   ", col.to[Buffer], testBuffer)
    printResult("[Direct] GenSeq   ", col.toSeq, testGenSeq)
    printResult("[Copy]   GenSeq   ", col.to[GenSeq], testGenSeq)
    printResult("[Copy]   Seq      ", col.to[Seq], testSeq)
    printResult("[Direct] Stream   ", col.toStream, testStream)
    printResult("[Copy]   Stream   ", col.to[Stream], testStream)
    printResult("[Direct] Array    ", col.toArray, testArray)
    printResult("[Copy]   Array    ", col.to[Array], testArray)
    printResult("[Copy]   ParVector", col.to[ParVector], testParVector)
    printResult("[Copy]   ParArray ", col.to[ParArray], testParArray)
  }

  def main(args: Array[String]): Unit = {
    testConversion("iterator", (1 to 3).iterator)
    testConversion("Vector", Vector(1,2,3))
    testConversion("List", List(1,2,3))
    testConversion("Buffer", Buffer(1,2,3))
    testConversion("ParVector", ParVector(1,2,3))
    testConversion("ParArray", ParArray(1,2,3))
    testConversion("Set", Set(1,2,3))
    testConversion("SetView", Set(1,2,3).view)
    testConversion("BufferView", Buffer(1,2,3).view)
  }
}

package scala

import org.junit.Assert._
import org.junit.Test

class ArrayTest {
  @Test
  def testUnapplySeq: Unit = {
    val array = Array(1, 2, 3)
    val seq = array match {
      case Array(xs @ _*) => xs
    }
    seq: scala.collection.immutable.Seq[Int] // compile success !?
  }
}

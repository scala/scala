package scala

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.RunTesting

class ExtractorTest extends RunTesting {
  val of: Int => Option[String] = { x =>
    if (x == 0) {
      Some("zero")
    } else if (x == 1) {
      Some("one")
    } else {
      None
    }
  }

  @Test def testOptionalFunctionExtractor(): Unit = {
    1 match {
      case of.unlift(m) =>
        assertEquals(m, "one")
    }
  }

  @Test
  def testOptionalFunctionExtractorMatchError(): Unit = {
    assertThrows(classOf[MatchError], () => 2 match {
      case of.unlift(m) =>
    })
  }

  @Test def testOptionalFunctionExtractorSeq(): Unit = {
    Seq(0, 1) match {
      case of.unlift.elementWise(m0, m1) =>
        assertEquals(m0, "zero")
        assertEquals(m1, "one")
    }
  }

  @Test
  def testOptionalFunctionExtractorSeqMatchError(): Unit = {
    assertThrows(classOf[MatchError], () => Seq(1, 2) match {
      case of.unlift.elementWise(m0, m1) =>
    })
  }

  val pf: PartialFunction[Int, String] = {
    case 0 => "zero"
    case 1 => "one"
  }

  @Test def testPartialFunctionExtractor(): Unit = {
    1 match {
      case pf(m) =>
        assertEquals(m, "one")
    }
  }

  @Test
  def testPartialFunctionExtractorMatchError(): Unit = {
    assertThrows(classOf[MatchError], () => 2 match {
      case pf(m) =>
    })
  }

  @Test def testPartialFunctionExtractorSeq(): Unit = {
    Seq(0, 1) match {
      case pf.elementWise(m0, m1) =>
        assertEquals(m0, "zero")
        assertEquals(m1, "one")
    }
  }

  @Test
  def testPartialFunctionExtractorSeqMatchError(): Unit = {
    assertThrows(classOf[MatchError], () => Seq(1, 2) match {
      case pf.elementWise(m0, m1) =>
    })
  }

}

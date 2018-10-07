package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

@RunWith(classOf[JUnit4])
class MapTest {
  @Test def test: Unit = {
    val map = collection.Map(
      1 -> 1,
      2 -> 2,
      4 -> 4,
      5 -> 5
    )

    val actual = map -- List(1, 2, 3)

    val expected = collection.Map(
      4 -> 4,
      5 -> 5
    )

    assertEquals(expected, actual)
  }

  @Test def mkString(): Unit = {
    assert(Map().mkString == "")
    assert(Map(1 -> 1).mkString(",") == "1 -> 1")
    assert(Map(1 -> 1, 2 -> 2).mkString(",") == "1 -> 1,2 -> 2")
  }

  @Test def addString(): Unit = {
    assert(Map().addString(new StringBuilder).toString == "")
    assert(Map(1 -> 1).addString(new StringBuilder).toString == "1 -> 1")
    assert(Map(1 -> 1, 2 -> 2).mkString("foo [", ", ", "] bar").toString ==
      "foo [1 -> 1, 2 -> 2] bar")
  }

  @Test def t11188(): Unit = {
    import scala.collection.immutable.ListMap

    val m = ListMap(1 -> "one")
    val mm = Map(2 -> "two") ++: m
    assert(mm.isInstanceOf[ListMap[Int,String]])
    assertEquals(mm.mkString("[", ", ", "]"), "[1 -> one, 2 -> two]")
  }
}
package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MapLikeTest {

  val ma = Map(0 -> " ", 1 -> "a", 2 -> "b")
  val mb = Map(1 -> "A", 2 -> "B", 3 -> "C")

  @Test
  def joinInnerRightValues(): Unit = {

    assertEquals(Map(1 -> ("a", "A"), 2 -> ("b", "B")), ma joinInner mb)

  }

  @Test
  def joinOuterLeftRightValues(): Unit = {

    val expected = Map(
      0 -> (" ", None     ),
      1 -> ("a", Some("A")),
      2 -> ("b", Some("B"))
    )

    assertEquals(expected, ma joinOuterLeft mb)

  }

  @Test
  def joinOuterRightRightValues(): Unit = {

    val expected = Map(
      1 -> (Some("a"), "A"),
      2 -> (Some("b"), "B"),
      3 -> (None     , "C")
    )

    assertEquals(expected, ma joinOuterRight mb)

  }

}

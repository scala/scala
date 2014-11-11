package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TreeMapTest {

  @Test
  def hasCorrectDropAndTakeMethods() {
    val tree = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")

    assertEquals(TreeMap.empty[Int, String], tree take Int.MinValue)
    assertEquals(TreeMap.empty[Int, String], tree takeRight Int.MinValue)
    assertEquals(tree, tree drop Int.MinValue)
    assertEquals(tree, tree dropRight Int.MinValue)
  }
}

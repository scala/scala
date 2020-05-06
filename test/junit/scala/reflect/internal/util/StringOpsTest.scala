package scala.reflect.internal.util

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StringOpsTest {
  @Test
  def prefixOfNone(): Unit = {
    val so = new StringOps { }
    val ss = Nil
    val lcp = so longestCommonPrefix ss
    assertEquals("", lcp)
  }
  @Test
  def prefixWithEmpty(): Unit = {
    val so = new StringOps { }
    val ss = List("abc", "", "abd")
    val lcp = so longestCommonPrefix ss
    assertEquals("", lcp)
  }
  @Test
  def prefixOfOne(): Unit = {
    val so = new StringOps { }
    val ss = List("abc")
    val lcp = so longestCommonPrefix ss
    assertEquals("abc", lcp)
  }
  @Test
  def prefixOfMany(): Unit = {
    val so = new StringOps { }
    val ss = List("abc", "abd", "abe")
    val lcp = so longestCommonPrefix ss
    assertEquals("ab", lcp)
  }
  @Test
  def prefixOfPrefix(): Unit = {
    val so = new StringOps { }
    val ss = List("abc", "abcd")
    val lcp = so longestCommonPrefix ss
    assertEquals("abc", lcp)
  }
  @Test
  def prefixOfPrefixMiddling(): Unit = {
    val so = new StringOps { }
    val ss = List("abce", "abc", "abcd")
    val lcp = so longestCommonPrefix ss
    assertEquals("abc", lcp)
  }
}

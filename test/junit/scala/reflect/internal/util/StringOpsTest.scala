package scala.reflect.internal.util

import org.junit.Assert._
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
    assert(lcp == "")
  }
  @Test
  def prefixWithEmpty(): Unit = {
    val so = new StringOps { }
    val ss = List("abc", "", "abd")
    val lcp = so longestCommonPrefix ss
    assert(lcp == "")
  }
  @Test
  def prefixOfOne(): Unit = {
    val so = new StringOps { }
    val ss = List("abc")
    val lcp = so longestCommonPrefix ss
    assert(lcp == "abc")
  }
  @Test
  def prefixOfMany(): Unit = {
    val so = new StringOps { }
    val ss = List("abc", "abd", "abe")
    val lcp = so longestCommonPrefix ss
    assert(lcp == "ab")
  }
  @Test
  def prefixOfPrefix(): Unit = {
    val so = new StringOps { }
    val ss = List("abc", "abcd")
    val lcp = so longestCommonPrefix ss
    assert(lcp == "abc")
  }
  @Test
  def prefixOfPrefixMiddling(): Unit = {
    val so = new StringOps { }
    val ss = List("abce", "abc", "abcd")
    val lcp = so longestCommonPrefix ss
    assert(lcp == "abc")
  }
}

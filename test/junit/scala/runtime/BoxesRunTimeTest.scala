package scala.runtime

import org.junit.Assert._
import org.junit.Test

import scala.annotation.nowarn

@nowarn("cat=other-non-cooperative-equals")
class BoxesRunTimeTest {
  @Test def nullNumberEqualsChar(): Unit = {
    val i: java.lang.Integer = null
    val n: java.lang.Number = null
    val c: Char = 'a'
    val bc: java.lang.Character = 'a'
    val any: Any = 'a'
    // receiver statically a Number, null at run time, argument a Char
    assertFalse(i == c)
    assertFalse(i == bc)
    assertFalse(i == any)
    assertTrue(n != c)
    assertFalse(BoxesRunTime.equalsNumChar(null, bc))
    assertFalse(BoxesRunTime.equalsNumObject(null, bc))
    assertTrue(BoxesRunTime.equalsNumChar(null, null))
    // the same comparison through Any
    assertFalse((i: Any) == (c: Any))
    // the mirrored call and the non-Char cases
    assertFalse(c == i)
    assertFalse(i == 1L)
    assertFalse(i == ("a": Any))
  }
}

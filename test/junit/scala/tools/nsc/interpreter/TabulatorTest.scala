package scala.tools.nsc.interpreter.shell

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

case class Tabby(width: Int = 80, isAcross: Boolean = false, marginSize: Int = 3) extends Tabulator
case class VTabby(width: Int = 80, isAcross: Boolean = false, marginSize: Int = 3) extends VariColumnTabulator

class TabulatorTest {

  // à̧̄ is composed using 3 combining characters
  val complexA = "a\u0300\u0304\u0327"
  // 𝔟 is stored as 2 characters, but displayed as 1 grapheme
  val frakturB = "\uD835\uDD1F"

  @Test def oneliner() = {
    val sut   = Tabby()
    val items = List(complexA, frakturB, "c")
    val res   = sut tabulate items
    assert(res.size == 1)
    assert(res(0).size == 1)
    assert(res(0)(0) startsWith complexA)
    assert(res(0)(0) endsWith "c")
  }
  @Test def twoliner() = {
    val sut   = Tabby(width = 40)
    val items = List(complexA * 15, frakturB * 15, "c" * 15)
    val res   = sut tabulate items
    assert(res.size == 2)
    assert(res(0).size == 2)
    assert(res(1).size == 2)          // trailing empty strings
    assert(res(1)(0) startsWith frakturB)
  }
  @Test def twolinerx() = {
    val sut   = Tabby(width = 40, isAcross = true)
    val items = List(complexA * 15, frakturB * 15, "c" * 15)
    val res   = sut tabulate items
    assertEquals(res.size, 2)
    assertEquals(res(0).size, 2)
    assertEquals(res(1).size, 1)          // no trailing empty strings
    assert(res(1)(0) startsWith "c")
  }
  // before, two 9-width cols don't fit in 20
  // but now, 5-col and 9-col do fit.
  @Test def twolinerVariable() = {
    val sut   = VTabby(width = 20)
    val items = (1 to 9) map (i => i.toString * i)
    val rows  = sut tabulate items
    assertEquals(rows.size, 5)
    assertEquals(rows(0).size,2)
    assertEquals(rows(0)(0).size, 8) // width is 55555 plus margin of 3
  }
  @Test def sys() = {
    val sut   = VTabby(width = 40)
    val items = List("BooleanProp", "PropImpl", "addShutdownHook", "error",
                    "process", "CreatorImpl", "ShutdownHookThread", "allThreads",
                    "exit", "props", "Prop", "SystemProperties",
                    "env", "package", "runtime")
    val rows  = sut tabulate items
    assertEquals(rows.size, 8)
    assertEquals(rows(0).size, 2)
    assertEquals(rows(0)(0).size, "ShutdownHookThread".length + sut.marginSize)   // 21
  }
  @Test def syswide() = {
    val sut   = VTabby(width = 120)
    val items = List("BooleanProp", "PropImpl", "addShutdownHook", "error",
                    "process", "CreatorImpl", "ShutdownHookThread", "allThreads",
                    "exit", "props", "Prop", "SystemProperties",
                    "env", "package", "runtime")
    val rows  = sut tabulate items
    assertEquals(rows.size, 2)
    assertEquals(rows(0).size, 8)
    assertEquals(rows(0)(0).size, "BooleanProp".length + sut.marginSize)  // 14
  }
  @Test def resultFits() = {
    val sut   = VTabby(width = 10)
    // each of two lines would fit, but layout is two cols of width six > 10
    // therefore, should choose ncols = 1
    val items = List(complexA, "bcd",
                    "efg", frakturB)
    val rows  = sut tabulate items
    assertEquals(rows.size, 4)
    assertEquals(rows(0).size, 1)
    assertEquals(rows(0)(0).size, complexA.length + 2 + sut.marginSize)
  }
  @Test def badFit() = {
    val sut = VTabby(isAcross = true)
    val items = ('a' until 'z').map(_.toString).toList
    val rows = sut tabulate items
    assertEquals(rows.size, 2)
    assertEquals(rows(0).size, 20)   // 20 * 4 = 80
    assertEquals(rows(1)(0).dropRight(sut.marginSize), "u")
  }
  @Test def badFitter() = {
    val sut = VTabby(isAcross = true)
    val items = List (
      "%", "&", "*", "+", "-", "/", ">", ">=", ">>", ">>>", "^",
      "asInstanceOf", "isInstanceOf", "toByte", "toChar", "toDouble", "toFloat",
      "toInt", "toLong", "toShort", "toString", "unary_+", "unary_-", "unary_~", "|"
    )
    val rows = sut tabulate items
    assertEquals(rows.size, 4)
    assertEquals(rows(3).size, 4)   // 7 cols
    assertEquals(rows(3)(0).dropRight(sut.marginSize), "unary_+")
  }
}

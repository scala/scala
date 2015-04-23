package scala.tools.nsc
package interpreter

//import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

case class Tabby(width: Int = 80, isAcross: Boolean = false, marginSize: Int = 3) extends Tabulator
case class VTabby(width: Int = 80, isAcross: Boolean = false, marginSize: Int = 3) extends VariColumnTabulator

@RunWith(classOf[JUnit4])
class TabulatorTest {

  @Test def oneliner() = {
    val sut   = Tabby()
    val items = List("a", "b", "c")
    val res   = sut tabulate items
    assert(res.size == 1)
    assert(res(0).size == 1)
    assert(res(0)(0) startsWith "a")
    assert(res(0)(0) endsWith "c")
  }
  @Test def twoliner() = {
    val sut   = Tabby(width = 40)
    val items = List("a" * 15, "b" * 15, "c" * 15)
    val res   = sut tabulate items
    assert(res.size == 2)
    assert(res(0).size == 2)
    assert(res(1).size == 2)          // trailing empty strings
    assert(res(1)(0) startsWith "b")
  }
  @Test def twolinerx() = {
    val sut   = Tabby(width = 40, isAcross = true)
    val items = List("a" * 15, "b" * 15, "c" * 15)
    val res   = sut tabulate items
    assert(res.size == 2)
    assert(res(0).size == 2)
    assert(res(1).size == 1)          // no trailing empty strings
    assert(res(1)(0) startsWith "c")
  }
  // before, two 9-width cols don't fit in 20
  // but now, 5-col and 9-col do fit.
  @Test def twolinerVariable() = {
    val sut   = VTabby(width = 20)
    val items = (1 to 9) map (i => i.toString * i)
    val rows  = sut tabulate items
    assert(rows.size == 5)
    assert(rows(0).size == 2)
    assert(rows(0)(0).size == 8) // width is 55555 plus margin of 3
  }
  @Test def sys() = {
    val sut   = VTabby(width = 40)
    val items = List("BooleanProp", "PropImpl", "addShutdownHook", "error",
                    "process", "CreatorImpl", "ShutdownHookThread", "allThreads",
                    "exit", "props", "Prop", "SystemProperties",
                    "env", "package", "runtime")
    val rows  = sut tabulate items
    assert(rows.size == 8)
    assert(rows(0).size == 2)
    assert(rows(0)(0).size == "ShutdownHookThread".length + sut.marginSize)   // 21
  }
  @Test def syswide() = {
    val sut   = VTabby(width = 120)
    val items = List("BooleanProp", "PropImpl", "addShutdownHook", "error",
                    "process", "CreatorImpl", "ShutdownHookThread", "allThreads",
                    "exit", "props", "Prop", "SystemProperties",
                    "env", "package", "runtime")
    val rows  = sut tabulate items
    assert(rows.size == 2)
    assert(rows(0).size == 8)
    assert(rows(0)(0).size == "BooleanProp".length + sut.marginSize)  // 14
  }
  @Test def resultFits() = {
    val sut   = VTabby(width = 10)
    // each of two lines would fit, but layout is two cols of width six > 10
    // therefore, should choose ncols = 1
    val items = List("a", "bcd",
                    "efg", "h")
    val rows  = sut tabulate items
    assert(rows.size == 4)
    assert(rows(0).size == 1)
    assert(rows(0)(0).size == "efg".length + sut.marginSize)  // 6
  }
  @Test def badFit() = {
    val sut = VTabby(isAcross = true)
    val items = ('a' until 'z').map(_.toString).toList
    val rows = sut tabulate items
    assert(rows.size == 2)
    assert(rows(0).size == 20)   // 20 * 4 = 80
    assert(rows(1)(0).dropRight(sut.marginSize) == "u")
  }
  @Test def badFitter() = {
    val sut = VTabby(isAcross = true)
    val items = List (
      "%", "&", "*", "+", "-", "/", ">", ">=", ">>", ">>>", "^",
      "asInstanceOf", "isInstanceOf", "toByte", "toChar", "toDouble", "toFloat",
      "toInt", "toLong", "toShort", "toString", "unary_+", "unary_-", "unary_~", "|"
    )
    val rows = sut tabulate items
    assert(rows.size == 4)
    assert(rows(3).size == 4)   // 7 cols
    assert(rows(3)(0).dropRight(sut.marginSize) == "unary_+")
  }
}

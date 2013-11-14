package scala.tools.nsc
package interpreter

//import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

case class Tabby(width: Int = 80, isAcross: Boolean = false, marginSize: Int = 3) extends Tabulator

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
}

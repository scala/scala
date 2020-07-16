package scala.tools
package testing

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class AssertThrowsTest {
  class Foo extends Exception
  class SubFoo extends Foo
  class Bar extends Exception

  @Test
  def catchFoo = assertThrows[Foo] { throw new Foo }

  @Test
  def catchSubclass = assertThrows[Foo] { throw new SubFoo }

  @Test
  def wrongThrow =
    assertTrue("Wrong exception thrown", {
      try {
        assertThrows[Foo] { throw new Bar }
        false
      } catch {
        case _: Bar => fail("Bar shouldn't have been rethrown"); false
        case _: AssertionError => true
        case t: Throwable => fail(s"expected AssertionError but got $t"); false
      }
    })

  @Test
  def errorIfNoThrow: Unit = {
    try {
      assertThrows[Foo] { () }
    } catch {
      case e: AssertionError => return
    }
    fail("assertThrows should error if the tested expression does not throw anything")
  }
}

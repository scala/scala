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
  class Bar extends Exception

  @Test
  def catchFoo = assertThrows[Foo] { throw new Foo }

  @Test
  def rethrowBar =
    try assertThrows[Foo] { throw new Bar }
    catch {
      case bar: Bar =>
      case e: Throwable => fail(s"expected Bar but got $e")
    }
}
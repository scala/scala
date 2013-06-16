package scala.tools.nsc
package test

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/** Sample JUnit test that shows that all pieces
    of JUnit infrastructure work correctly */
@RunWith(classOf[JUnit4])
class SampleTest {
  @Test
  def testMath: Unit = {
    assert(2+2 == 4, "you didn't get the math right fellow")
  }
}

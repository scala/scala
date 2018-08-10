package scala

import scala.tools.reflect.ToolBoxError
import scala.tools.testing.RunTesting

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StringTest extends RunTesting {
  @Test(expected = classOf[ToolBoxError]) def testNoSelf: Unit = runner.run(""" "A".self """)
}

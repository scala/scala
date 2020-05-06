package scala

import scala.tools.reflect.ToolBoxError
import scala.tools.testkit.AssertUtil._
import scala.tools.testkit.RunTesting

import org.junit.Test

class StringTest extends RunTesting {
  @Test def testNoSelf(): Unit   = assertThrows[ToolBoxError](runner.run(""" "A".self """))
  @Test def testNoUnwrap(): Unit = assertThrows[ToolBoxError](runner.run(""" "A".unwrap """))
}

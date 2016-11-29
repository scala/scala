package scala.lang.traits

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.RunTesting

@RunWith(classOf[JUnit4])
class RunTest extends RunTesting {
  import runner._

  @Test
  def invocationReceivers(): Unit = {
    import invocationReceiversTestCode._
    assertEquals(run[String](definitions("Object") + runCode), "hi" * 9)
    assertEquals(run[String](definitions("String") + runCode), "hi" * 9) // bridge method for clone generated
  }
}

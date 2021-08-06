package scala.lang.traits

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.RunTesting

class RunTest extends RunTesting {
  import runner._

  @Test
  def invocationReceivers(): Unit = {
    import invocationReceiversTestCode._
    assertEquals(run[String](definitions("Object") + runCode), "hi" * 9)
    assertEquals(run[String](definitions("String") + runCode), "hi" * 9) // bridge method for clone generated
  }
}

package scala.tools.xsbt

import org.junit.Test
import xsbti.InteractiveConsoleResult

class InteractiveConsoleInterfaceTest extends BridgeTesting {

  @Test
  def `Scala interpreter should evaluate arithmetic expression`(): Unit = {
    withInteractiveConsole { repl =>
      val response = repl.interpret("1+1", false)
      assert(response.output.trim == "val res0: Int = 2")
      assert(response.result == InteractiveConsoleResult.Success)
    }
  }

  @Test
  def `it should evaluate list constructor`(): Unit = {
    withInteractiveConsole { repl =>
      val response = repl.interpret("List(1,2)", false)
      assert(response.output.trim == "val res0: List[Int] = List(1, 2)")
      assert(response.result == InteractiveConsoleResult.Success)
    }
  }

  @Test
  def `it should evaluate import`(): Unit = {
    withInteractiveConsole { repl =>
      val response = repl.interpret("import scala.collection.mutable._", false)
      assert(response.output.trim == "import scala.collection.mutable._")
      assert(response.result == InteractiveConsoleResult.Success)
    }
  }

  @Test
  def `it should mark partial expression as incomplete`(): Unit = {
    withInteractiveConsole { repl =>
      val response = repl.interpret("val a =", false)
      assert(response.result == InteractiveConsoleResult.Incomplete)
    }
  }

  @Test
  def `it should not evaluate incorrect expression`(): Unit = {
    withInteractiveConsole { repl =>
      val response = repl.interpret("1 ++ 1", false)
      assert(response.result == InteractiveConsoleResult.Error)
    }
  }

  val postfixOpExpression = "import scala.concurrent.duration._\nval t = 1 second"

  @Test
  def `it should evaluate postfix op without warning when -language:postfixOps arg passed`(): Unit = withTemporaryDirectory { tempDir =>
    val repl = interactiveConsole(tempDir.toPath)("-language:postfixOps")
    try {
      val response = repl.interpret(postfixOpExpression, false)
      assert(!response.output.trim.startsWith("warning"))
      assert(response.result == InteractiveConsoleResult.Success)
    } finally {
      repl.close()
    }
  }
}

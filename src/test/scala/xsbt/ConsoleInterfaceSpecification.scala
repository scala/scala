package xsbt

import sbt.internal.util.UnitSpec
import sbt.util.Logger
import xsbti.ConsoleResult

// This is a specification to check the REPL block parsing.
class ConsoleInterfaceSpecification extends UnitSpec {

  private val consoleFactory = new ConsoleFactory

  def consoleWithArgs(args: String*) = consoleFactory.createConsole(
    args = args.toArray,
    bootClasspathString = "",
    classpathString = "",
    initialCommands = "",
    cleanupCommands = "",
    loader = this.getClass.getClassLoader,
    bindNames = Array.empty,
    bindValues = Array.empty,
    log = Logger.Null
  )

  private val consoleWithoutArgs = consoleWithArgs()

  "Scala interpreter" should "evaluate arithmetic expression" in {
    val response = consoleWithoutArgs.interpret("1+1", false)
    response.output.trim shouldBe "res0: Int = 2"
    response.result shouldBe ConsoleResult.Success
  }

  it should "evaluate list constructor" in {
    val response = consoleWithoutArgs.interpret("List(1,2)", false)
    response.output.trim shouldBe "res1: List[Int] = List(1, 2)"
    response.result shouldBe ConsoleResult.Success
  }

  it should "evaluate import" in {
    val response = consoleWithoutArgs.interpret("import xsbt._", false)
    response.output.trim shouldBe "import xsbt._"
    response.result shouldBe ConsoleResult.Success
  }

  it should "mark partial expression as incomplete" in {
    val response = consoleWithoutArgs.interpret("val a =", false)
    response.result shouldBe ConsoleResult.Incomplete
  }

  it should "not evaluate incorrect expression" in {
    val response = consoleWithoutArgs.interpret("1 ++ 1", false)
    response.result shouldBe ConsoleResult.Error
  }

  val postfixOpExpression = "import scala.concurrent.duration._\nval t = 1 second"

  it should "evaluate postfix op with a warning" in {
    val response = consoleWithoutArgs.interpret(postfixOpExpression, false)
    response.output.trim should startWith("warning")
    response.result shouldBe ConsoleResult.Success
  }

  private val consoleWithPostfixOps = consoleWithArgs("-language:postfixOps")

  it should "evaluate postfix op without warning when -language:postfixOps arg passed" in {
    val response = consoleWithPostfixOps.interpret(postfixOpExpression, false)
    response.output.trim should not startWith "warning"
    response.result shouldBe ConsoleResult.Success
  }

}

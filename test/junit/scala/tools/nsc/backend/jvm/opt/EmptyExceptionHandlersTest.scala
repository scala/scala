package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._

@RunWith(classOf[JUnit4])
class EmptyExceptionHandlersTest {

  val exceptionDescriptor = "java/lang/Exception"

  @Test
  def eliminateEmpty(): Unit = {
    val handlers = List(ExceptionHandler(Label(1), Label(2), Label(2), Some(exceptionDescriptor)))
    val asmMethod = genMethod(handlers = handlers)(
      Label(1),
      Label(2),
      Op(RETURN)
    )
    assertTrue(convertMethod(asmMethod).handlers.length == 1)
    LocalOpt.removeEmptyExceptionHandlers(asmMethod)
    assertTrue(convertMethod(asmMethod).handlers.isEmpty)
  }

  @Test
  def eliminateHandlersGuardingNops(): Unit = {
    val handlers = List(ExceptionHandler(Label(1), Label(2), Label(2), Some(exceptionDescriptor)))
    val asmMethod = genMethod(handlers = handlers)(
      Label(1),          // nops only
      Op(NOP),
      Op(NOP),
      Jump(GOTO, Label(3)),
      Op(NOP),
      Label(3),
      Op(NOP),
      Jump(GOTO, Label(4)),

      Label(2),          // handler
      Op(ACONST_NULL),
      Op(ATHROW),

      Label(4),          // return
      Op(RETURN)
    )
    assertTrue(convertMethod(asmMethod).handlers.length == 1)
    LocalOpt.removeEmptyExceptionHandlers(asmMethod)
    assertTrue(convertMethod(asmMethod).handlers.isEmpty)
  }

  val noOptCompiler       = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:l:none")
  val dceCompiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:unreachable-code")

  @Test
  def eliminateUnreachableHandler(): Unit = {
    val code = "def f: Unit = try { } catch { case _: Exception => println(0) }; println(1)"

    assertTrue(singleMethod(noOptCompiler)(code).handlers.length == 1)
    val optMethod = singleMethod(dceCompiler)(code)
    assertTrue(optMethod.handlers.isEmpty)

    val code2 =
      """def f: Unit = {
        |  println(0)
        |  return
        |  try { throw new Exception("") } // removed by dce, so handler will be removed as well
        |  catch { case _: Exception => println(1) }
        |  println(2)
        |}""".stripMargin

    assertTrue(singleMethod(dceCompiler)(code2).handlers.isEmpty)
  }

  @Test
  def keepAliveHandlers(): Unit = {
    val code =
      """def f: Int = {
        |  println(0)
        |  try { 1 }
        |  catch { case _: Exception => 2 }
        |}""".stripMargin

    assertTrue(singleMethod(dceCompiler)(code).handlers.length == 1)
  }
}

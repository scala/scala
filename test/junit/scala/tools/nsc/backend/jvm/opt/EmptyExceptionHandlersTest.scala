package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._


@RunWith(classOf[JUnit4])
class EmptyExceptionHandlersTest extends BytecodeTesting {
  override def compilerArgs = "-opt:unreachable-code"
  def dceCompiler = compiler

  val noOptCompiler = cached("noOptCompiler", () => newCompiler(extraArgs = "-opt:l:none"))

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
    LocalOptImpls.removeEmptyExceptionHandlers(asmMethod)
    assertTrue(convertMethod(asmMethod).handlers.isEmpty)
  }

  @Test
  def eliminateHandlersGuardingNops(): Unit = {
    val handlers = List(ExceptionHandler(Label(1), Label(2), Label(2), Some(exceptionDescriptor)))
    val asmMethod = genMethod(handlers = handlers)(
      Label(1),          // nops only
      Jump(GOTO, Label(3)),
      Label(3),
      Jump(GOTO, Label(4)),

      Label(2),          // handler
      Op(ACONST_NULL),
      Op(ATHROW),

      Label(4),          // return
      Op(RETURN)
    )
    assertTrue(convertMethod(asmMethod).handlers.length == 1)
    LocalOptImpls.removeEmptyExceptionHandlers(asmMethod)
    assertTrue(convertMethod(asmMethod).handlers.isEmpty)
  }

  @Test
  def eliminateUnreachableHandler(): Unit = {
    val code = "def f: Unit = try { } catch { case _: Exception => println(0) }; println(1)"

    assertTrue(noOptCompiler.compileMethod(code).handlers.length == 1)
    val optMethod = dceCompiler.compileMethod(code)
    assertTrue(optMethod.handlers.isEmpty)

    val code2 =
      """def f: Unit = {
        |  println(0)
        |  return
        |  try { throw new Exception("") } // removed by dce, so handler will be removed as well
        |  catch { case _: Exception => println(1) }
        |  println(2)
        |}""".stripMargin

    assertTrue(dceCompiler.compileMethod(code2).handlers.isEmpty)
  }

  @Test
  def keepAliveHandlers(): Unit = {
    val code =
      """def f: Int = {
        |  println(0)
        |  try { 1 }
        |  catch { case _: Exception => 2 }
        |}""".stripMargin

    assertTrue(dceCompiler.compileMethod(code).handlers.length == 1)
  }
}

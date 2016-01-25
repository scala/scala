package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.generic.Clearable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.io._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._

import BackendReporting._

import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object ClosureOptimizerTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler(extraArgs = "-Yopt:l:classpath -Yopt-warnings:_")
  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class ClosureOptimizerTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = ClosureOptimizerTest

  val compiler = ClosureOptimizerTest.compiler

  @Test
  def nothingTypedClosureBody(): Unit = {
    val code =
      """abstract class C {
        |  def isEmpty: Boolean
        |  @inline final def getOrElse[T >: C](f: => T) = if (isEmpty) f else this
        |  def t = getOrElse(throw new Error(""))
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    val t = findAsmMethod(c, "t")
    val List(bodyCall) = findInstr(t, "INVOKESTATIC C.C$$$anonfun$1 ()Lscala/runtime/Nothing$")
    assert(bodyCall.getNext.getOpcode == ATHROW)
  }

  @Test
  def nullTypedClosureBody(): Unit = {
    val code =
      """abstract class C {
        |  def isEmpty: Boolean
        |  @inline final def getOrElse[T >: C](f: => T) = if (isEmpty) f else this
        |  def t = getOrElse(null)
        |}
      """.stripMargin

    val List(c) = compileClasses(compiler)(code)
    val t = findAsmMethod(c, "t")
    val List(bodyCall) = findInstr(t, "INVOKESTATIC C.C$$$anonfun$1 ()Lscala/runtime/Null$")
    assert(bodyCall.getNext.getOpcode == POP)
    assert(bodyCall.getNext.getNext.getOpcode == ACONST_NULL)
  }

  @Test
  def makeLMFCastExplicit(): Unit = {
    val code =
      """class C {
        |  def t(l: List[String]) = {
        |    val fun: String => String = s => s
        |    fun(l.head)
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(compiler)(code)
    assertSameCode(getSingleMethod(c, "t").instructions.dropNonOp,
      List(VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "scala/collection/immutable/List", "head", "()Ljava/lang/Object;", false),
        TypeOp(CHECKCAST, "java/lang/String"), Invoke(INVOKESTATIC, "C", "C$$$anonfun$1", "(Ljava/lang/String;)Ljava/lang/String;", false),
        Op(ARETURN)))
  }

  @Test
  def closureOptWithUnreachableCode(): Unit = {
    // this example used to crash the ProdCons analysis in the closure optimizer - ProdCons
    // expects no unreachable code.
    val code =
      """class C {
        |  @inline final def m = throw new Error("")
        |  def t = {
        |    val f = (x: Int) => x + 1
        |    m
        |    f(10) // unreachable after inlining m
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(compiler)(code)
    assertEquals(getSingleMethod(c, "t").instructions.summary,
      List(NEW, DUP, LDC, "<init>", ATHROW))
  }
}

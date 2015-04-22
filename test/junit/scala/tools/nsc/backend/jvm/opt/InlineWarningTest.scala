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
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils.AsmAnalyzer
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

object InlineWarningTest extends ClearAfterClass.Clearable {
  val argsNoWarn = "-Ybackend:GenBCode -Yopt:l:classpath"
  val args = argsNoWarn + " -Yopt-warnings"
  var compiler = newCompiler(extraArgs = args)
  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class InlineWarningTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = InlineWarningTest

  val compiler = InlineWarningTest.compiler

  def compile(scalaCode: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): List[ClassNode] = {
    compileClasses(compiler)(scalaCode, javaCode, allowMessage)
  }

  @Test
  def nonFinal(): Unit = {
    val code =
      """class C {
        |  @inline def m1 = 1
        |}
        |trait T {
        |  @inline def m2 = 1
        |}
        |class D extends C with T
        |
        |class Test {
        |  def t1(c: C, t: T, d: D) = c.m1 + t.m2 + d.m1 + d.m2
        |}
      """.stripMargin
    var count = 0
    val warns = Set(
      "C::m1()I is annotated @inline but cannot be inlined: the method is not final and may be overridden",
      "T::m2()I is annotated @inline but cannot be inlined: the method is not final and may be overridden",
      "D::m2()I is annotated @inline but cannot be inlined: the method is not final and may be overridden")
    compile(code, allowMessage = i => {count += 1; warns.exists(i.msg contains _)})
    assert(count == 4, count)
  }

  @Test
  def traitMissingImplClass(): Unit = {
    val codeA = "trait T { @inline final def f = 1 }"
    val codeB = "class C { def t1(t: T) = t.f }"

    val removeImpl = (outDir: AbstractFile) => {
      val f = outDir.lookupName("T$class.class", directory = false)
      if (f != null) f.delete()
    }

    val warn =
      """T::f()I is annotated @inline but cannot be inlined: the trait method call could not be rewritten to the static implementation method. Possible reason:
        |The method f(LT;)I could not be found in the class T$class or any of its parents.
        |Note that the following parent classes could not be found on the classpath: T$class""".stripMargin

    var c = 0
    compileSeparately(List(codeA, codeB), extraArgs = InlineWarningTest.args, afterEach = removeImpl, allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)

    // only summary here
    compileSeparately(List(codeA, codeB), extraArgs = InlineWarningTest.argsNoWarn, afterEach = removeImpl, allowMessage = _.msg contains "there was one inliner warning")
  }

  @Test
  def handlerNonEmptyStack(): Unit = {
    val code =
      """class C {
        |  @noinline def q = 0
        |  @inline final def foo = try { q } catch { case e: Exception => 2 }
        |  def t1 = println(foo) // inline warning here: foo cannot be inlined on top of a non-empty stack
        |}
      """.stripMargin

    var c = 0
    compile(code, allowMessage = i => {c += 1; i.msg contains "operand stack at the callsite in C::t1()V contains more values"})
    assert(c == 1, c)
  }

  @Test
  def mixedWarnings(): Unit = {
    val javaCode =
      """public class A {
        |  public static final int bar() { return 100; }
        |}
      """.stripMargin

    val scalaCode =
      """class B {
        |  @inline final def flop = A.bar
        |  def g = flop
        |}
      """.stripMargin

    val warns = List(
      """failed to determine if bar should be inlined:
        |The method bar()I could not be found in the class A or any of its parents.
        |Note that the following parent classes are defined in Java sources (mixed compilation), no bytecode is available: A""".stripMargin,

      """B::flop()I is annotated @inline but could not be inlined:
        |Failed to check if B::flop()I can be safely inlined to B without causing an IllegalAccessError. Checking instruction INVOKESTATIC A.bar ()I failed:
        |The method bar()I could not be found in the class A or any of its parents.
        |Note that the following parent classes are defined in Java sources (mixed compilation), no bytecode is available: A""".stripMargin)

    var c = 0
    val List(b) = compile(scalaCode, List((javaCode, "A.java")), allowMessage = i => {c += 1; warns.tail.exists(i.msg contains _)})
    assert(c == 1, c)

    // no warnings here
    compileClasses(newCompiler(extraArgs = InlineWarningTest.argsNoWarn + " -Yopt-warnings:none"))(scalaCode, List((javaCode, "A.java")))

    c = 0
    compileClasses(newCompiler(extraArgs = InlineWarningTest.argsNoWarn + " -Yopt-warnings:no-inline-mixed"))(scalaCode, List((javaCode, "A.java")), allowMessage = i => {c += 1; warns.exists(i.msg contains _)})
    assert(c == 2, c)
  }

  @Test
  def cannotInlinePrivateCallIntoDifferentClass(): Unit = {
    val code =
      """class M {
        |  @inline final def f = {
        |    @noinline def nested = 0
        |    nested
        |  }
        |
        |  def t = f // ok
        |}
        |
        |class N {
        |  def t(a: M) = a.f // not possible
        |}
      """.stripMargin

    val warn =
      """M::f()I is annotated @inline but could not be inlined:
        |The callee M::f()I contains the instruction INVOKESPECIAL M.nested$1 ()I
        |that would cause an IllegalAccessError when inlined into class N""".stripMargin

    var c = 0
    compile(code, allowMessage = i => { c += 1; i.msg contains warn })
    assert(c == 1, c)
  }

  @Test
  def cannotMixStrictfp(): Unit = {
    val code =
      """import annotation.strictfp
        |class C {
        |  @strictfp @inline final def f = 0
        |  @strictfp def t1 = f
        |  def t2 = f
        |}
      """.stripMargin

    val warn =
      """C::f()I is annotated @inline but could not be inlined:
        |The callsite method C::t2()I
        |does not have the same strictfp mode as the callee C::f()I.""".stripMargin

    var c = 0
    compile(code, allowMessage = i => { c += 1; i.msg contains warn })
    assert(c == 1, c)
  }
}

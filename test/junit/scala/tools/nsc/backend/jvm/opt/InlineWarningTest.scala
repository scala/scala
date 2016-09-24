package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class InlineWarningTest extends BytecodeTesting {
  def optCp = "-opt:l:classpath"
  override def compilerArgs = s"$optCp -opt-warnings"

  import compiler._

  val compilerWarnAll = cached("compilerWarnAll", () => newCompiler(extraArgs = s"$optCp -opt-warnings:_"))

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
    compileToBytes(code, allowMessage = i => {count += 1; warns.exists(i.msg contains _)})
    assert(count == 4, count)
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
    compileToBytes(code, allowMessage = i => {c += 1; i.msg contains "operand stack at the callsite in C::t1()V contains more values"})
    assert(c == 1, c)
  }

//  @Test -- TODO
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
        |Note that class A is defined in a Java source (mixed compilation), no bytecode is available.""".stripMargin,

      """B::flop()I is annotated @inline but could not be inlined:
        |Failed to check if B::flop()I can be safely inlined to B without causing an IllegalAccessError. Checking instruction INVOKESTATIC A.bar ()I failed:
        |The method bar()I could not be found in the class A or any of its parents.
        |Note that class A is defined in a Java source (mixed compilation), no bytecode is available.""".stripMargin)

    var c = 0
    val List(b) = compileToBytes(scalaCode, List((javaCode, "A.java")), allowMessage = i => {c += 1; warns.tail.exists(i.msg contains _)})
    assert(c == 1, c)

    // no warnings here
    newCompiler(extraArgs = s"$optCp -opt-warnings:none").compileToBytes(scalaCode, List((javaCode, "A.java")))

    c = 0
    newCompiler(extraArgs = s"$optCp -opt-warnings:no-inline-mixed").compileToBytes(scalaCode, List((javaCode, "A.java")), allowMessage = i => {c += 1; warns.exists(i.msg contains _)})
    assert(c == 2, c)
  }

  @Test
  def cannotInlinePrivateCallIntoDifferentClass(): Unit = {
    val code =
      """class A {
        |  @inline final def f = {
        |    @noinline def nested = 0
        |    nested
        |  }
        |
        |  def t = f // ok
        |}
        |
        |class B {
        |  def t(a: A) = a.f // not possible
        |}
      """.stripMargin

    val warn =
      """A::f()I is annotated @inline but could not be inlined:
        |The callee A::f()I contains the instruction INVOKESTATIC A.nested$1 ()I
        |that would cause an IllegalAccessError when inlined into class B""".stripMargin

    var c = 0
    compileToBytes(code, allowMessage = i => { c += 1; i.msg contains warn })
    assert(c == 1, c)
  }

  @Test
  def dontWarnWhenNotIlnineAnnotated(): Unit = {
    val code =
      """class A {
        |  final def f(t: Int => Int) = {
        |    @noinline def nested = 0
        |    nested + t(1)
        |  }
        |  def t = f(x => x + 1)
        |}
        |
        |class B {
        |  def t(a: A) = a.f(x => x + 1)
        |}
      """.stripMargin
    compileToBytes(code, allowMessage = _ => false) // no warnings allowed

    val warn =
      """A::f(Lscala/Function1;)I could not be inlined:
        |The callee A::f(Lscala/Function1;)I contains the instruction INVOKESTATIC A.nested$1 ()I
        |that would cause an IllegalAccessError when inlined into class B""".stripMargin

    var c = 0
    compilerWarnAll.compileToBytes(code, allowMessage = i => { c += 1; i.msg contains warn })
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
    compileToBytes(code, allowMessage = i => { c += 1; i.msg contains warn })
    assert(c == 1, c)
  }

  @Test // scala-dev#20
  def mixedCompilationSpuriousWarning(): Unit = {
    val jCode =
      """public class A {
        |  public static final int bar() { return 100; }
        |  public final int baz() { return 100; }
        |}
      """.stripMargin

    val sCode =
      """class C {
        |  @inline final def foo = A.bar()
        |  @inline final def fii(a: A) = a.baz()
        |  def t = foo + fii(new A)
        |}
      """.stripMargin

    val warns = List(
      """C::foo()I is annotated @inline but could not be inlined:
        |Failed to check if C::foo()I can be safely inlined to C without causing an IllegalAccessError. Checking instruction INVOKESTATIC A.bar ()I failed:
        |The method bar()I could not be found in the class A or any of its parents.
        |Note that class A is defined in a Java source (mixed compilation), no bytecode is available.""".stripMargin,

      """C::fii(LA;)I is annotated @inline but could not be inlined:
        |Failed to check if C::fii(LA;)I can be safely inlined to C without causing an IllegalAccessError. Checking instruction INVOKEVIRTUAL A.baz ()I failed:
        |The method baz()I could not be found in the class A or any of its parents.
        |Note that class A is defined in a Java source (mixed compilation), no bytecode is available.""".stripMargin
    )
    var c = 0
    compileClasses(sCode, javaCode = List((jCode, "A.java")), allowMessage = i => { c += 1;
      warns.exists(i.msg.contains)
    })
    assert(c == 2)
  }
}

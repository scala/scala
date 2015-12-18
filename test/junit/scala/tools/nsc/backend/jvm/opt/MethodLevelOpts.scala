package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import scala.tools.testing.ClearAfterClass

object MethodLevelOpts extends ClearAfterClass.Clearable {
  var methodOptCompiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:l:method")
  def clear(): Unit = { methodOptCompiler = null }
}

@RunWith(classOf[JUnit4])
class MethodLevelOpts extends ClearAfterClass {
  ClearAfterClass.stateToClear = MethodLevelOpts

  val methodOptCompiler = MethodLevelOpts.methodOptCompiler

  def wrapInDefault(code: Instruction*) = List(Label(0), LineNumber(1, Label(0))) ::: code.toList ::: List(Label(1))

  @Test
  def eliminateEmptyTry(): Unit = {
    val code = "def f = { try {} catch { case _: Throwable => 0; () }; 1 }"
    val warn = "a pure expression does nothing in statement position"
    assertSameCode(singleMethodInstructions(methodOptCompiler)(code, allowMessage = _.msg contains warn), wrapInDefault(Op(ICONST_1), Op(IRETURN)))
  }

  @Test
  def eliminateLoadBoxedUnit(): Unit = {
    // the compiler inserts a boxed into the try block. it's therefore non-empty (and live) and not eliminated.
    val code = "def f = { try {} catch { case _: Throwable => 0 }; 1 }"
    val m = singleMethod(methodOptCompiler)(code)
    assertTrue(m.handlers.length == 0)
    assertSameCode(m.instructions.dropNonOp,
      List(Op(ICONST_1), Op(IRETURN)))
  }

  @Test
  def inlineThrowInCatchNotTry(): Unit = {
    // the try block does not contain the `ATHROW` instruction, but in the catch block, `ATHROW` is inlined
    val code = "def f(e: Exception) = throw { try e catch { case _: Throwable => e } }"
    val m = singleMethod(methodOptCompiler)(code)
    assertHandlerLabelPostions(m.handlers.head, m.instructions, 0, 3, 5)
    assertSameCode(m.instructions,
      wrapInDefault(VarOp(ALOAD, 1), Label(3), Op(ATHROW), Label(5), FrameEntry(4, List(), List("java/lang/Throwable")), Op(POP), VarOp(ALOAD, 1), Op(ATHROW))
    )
  }

  @Test
  def inlineReturnInCatchNotTry(): Unit = {
    val code = "def f: Int = return { try 1 catch { case _: Throwable => 2 } }"
    // cannot inline the IRETURN into the try block (because RETURN may throw IllegalMonitorState)
    val m = singleMethod(methodOptCompiler)(code)
    assertHandlerLabelPostions(m.handlers.head, m.instructions, 0, 3, 5)
    assertSameCode(m.instructions,
      wrapInDefault(Op(ICONST_1), Label(3), Op(IRETURN), Label(5), FrameEntry(4, List(), List("java/lang/Throwable")), Op(POP), Op(ICONST_2), Op(IRETURN)))
  }

  @Test
  def simplifyJumpsInTryCatchFinally(): Unit = {
    val code =
      """def f: Int =
        |  try {
        |    return 1
        |  } catch {
        |    case _: Throwable =>
        |    return 2
        |  } finally {
        |    return 3
        |    // dead
        |    val x = try 10 catch { case _: Throwable => 11 }
        |    println(x)
        |  }
      """.stripMargin
    val m = singleMethod(methodOptCompiler)(code)
    assertTrue(m.handlers.isEmpty)
    assertSameCode(m.instructions.dropNonOp, List(Op(ICONST_3), Op(IRETURN)))
  }

  @Test
  def nullStoreLoadElim(): Unit = {
    // point of this test: we have two cleanups
    //   - remove `ACONST_NULL; ASTORE x` if x is otherwise not live
    //   - remove `ASTORE x; ALOAD x` if x is otherwise not live
    // in the example below, we have `ACONST_NULL; ASTORE x; ALOAD x`. in this case the store-load
    // should be removed (even though it looks like a null-store at first).
    val code =
      """class C {
        |  def t = {
        |    val x = null
        |    x.toString
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)
    assertSameCode(getSingleMethod(c, "t").instructions.dropNonOp,
      List(Op(ACONST_NULL), Invoke(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;", false), Op(ARETURN)))
  }

  @Test
  def deadStoreReferenceElim(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    var a = "a"   // assign to non-initialized, directly removed by dead store
        |    a = "b"       // assign to initialized, replaced by null-store, which is then removed: the var is not live, the uses are null-store or store-load
        |    a = "c"
        |    a             // store-load pair will be eliminated
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)
    assertSameCode(
      getSingleMethod(c, "t").instructions.dropNonOp,
      List(Ldc(LDC, "c"), Op(ARETURN)))
  }

  @Test
  def deadStoreReferenceKeepNull(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    var a = "el"  // this store is live, used in the println.
        |    println(a)
        |    a = "met"     // since it's an ASTORE to a live variable, cannot elim the store (SI-5313), but store null instead.
        |                  // so we get `LDC met; POP; ACONST_NULL; ASTORE 1`. the `LDC met; POP` is eliminated by push-pop.
        |    a = "zit"     // this store is live, so we get `LDC zit; ASOTRE 1; ALOAD 1; ARETURN`.
        |                  // we cannot eliminated the store-load sequence, because the local is live (again SI-5313).
        |    a
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)

    assertEquals(
      getSingleMethod(c, "t").instructions.dropNonOp,
      List(
        Ldc(LDC, "el"), VarOp(ASTORE, 1),
        Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
        Op(ACONST_NULL), VarOp(ASTORE, 1),
        Ldc(LDC, "zit"), VarOp(ASTORE, 1), VarOp(ALOAD, 1), Op(ARETURN)))
  }

  @Test
  def elimUnusedTupleObjectStringBox(): Unit = {
    val code =
      """class C {
        |  def t(x: Int, y: Int): Int = {
        |    val a = (x, y)                            // Tuple2$mcII$sp
        |    val b = (a, y)                            // Tuple2
        |    val c = (new Object, "krik", new String)  // unused java/lang/Object, java/lang/String allocation and string constant is also eliminated
        |    val d = new java.lang.Integer(x)
        |    val e = new String(new Array[Char](23))
        |    val f = new scala.runtime.IntRef(11)
        |    x + y
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)
    assertEquals(getSingleMethod(c, "t").instructions.dropNonOp,
      List(VarOp(ILOAD, 1), VarOp(ILOAD, 2), Op(IADD), Op(IRETURN)))
  }

  @Test
  def noElimImpureConstructor(): Unit = {
    val code =
      """class C {
        |  def t(x: Int, y: Int): Int = {
        |    val a = new java.lang.Integer("nono")
        |    x + y
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)
    assertEquals(getSingleMethod(c, "t").instructions.dropNonOp,
      List(TypeOp(NEW, "java/lang/Integer"), Ldc(LDC, "nono"), Invoke(INVOKESPECIAL, "java/lang/Integer", "<init>", "(Ljava/lang/String;)V", false),
        VarOp(ILOAD, 1), VarOp(ILOAD, 2), Op(IADD), Op(IRETURN)))
  }

  @Test
  def elimUnusedBoxUnbox(): Unit = {
    val code =
      """class C {
        |  def t(a: Long): Int = {
        |    val t = 3 + a
        |    val u = a + t
        |    val v: Any = u // scala/runtime/BoxesRunTime.boxToLong
        |
        |    val w = (v, a) // a Tuple2 (not specialized because first value is Any)
        |                   // so calls scala/runtime/BoxesRunTime.boxToLong on the second value
        |
        |    val x = v.asInstanceOf[Long] // scala/runtime/BoxesRunTime.unboxToLong
        |
        |    val z = (java.lang.Long.valueOf(a), t)  // java box call on the left, scala/runtime/BoxesRunTime.boxToLong on the right
        |
        |    0
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)
    assertEquals(getSingleMethod(c, "t").instructions.dropNonOp,
      List(Op(ICONST_0), Op(IRETURN)))
  }

  @Test
  def elimUnusedClosure(): Unit = {
    val code =
      """class C {
        |  def t(x: Int, y: Int): Int = {
        |    val f = (a: Int) => a + x + y
        |    val g = (b: Int) => b - x
        |    val h = (s: String) => println(s)
        |    f(30)
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)
    assertEquals(
      getSingleMethod(c, "t").instructions.dropNonOp,
      List(
        IntOp(BIPUSH, 30), VarOp(ISTORE, 3),  // no constant propagation, so we keep the store (and load below) of a const
        VarOp(ILOAD, 1),
        VarOp(ILOAD, 2),
        VarOp(ILOAD, 3),
        Invoke(INVOKESTATIC, "C", "C$$$anonfun$1", "(III)I", false), Op(IRETURN)))
  }

  @Test
  def rewriteSpecializedClosureCall(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    val f1 = (x: Int) => println(x)       // int-unit specialization
        |    val f2 = (x: Int, y: Long) => x == y  // int-long-boolean
        |    f1(1)
        |    f2(3, 4)
        |  }
        |}
      """.stripMargin
    val List(c) = compileClasses(methodOptCompiler)(code)
    val t = getSingleMethod(c, "t")
    assert(!t.instructions.exists(_.opcode == INVOKEDYNAMIC), t)
  }
}

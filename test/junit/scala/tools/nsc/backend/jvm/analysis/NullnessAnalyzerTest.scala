package scala.tools.nsc
package backend.jvm
package analysis

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import CodeGenTools._
import scala.tools.asm.tree.{AbstractInsnNode, MethodNode}
import scala.tools.nsc.backend.jvm.BTypes._
import scala.tools.partest.ASMConverters
import ASMConverters._
import scala.tools.testing.ClearAfterClass
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import AsmUtils._

import scala.collection.convert.decorateAsScala._

object NullnessAnalyzerTest extends ClearAfterClass.Clearable {
  var noOptCompiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:l:none")

  def clear(): Unit = {
    noOptCompiler = null
  }
}

@RunWith(classOf[JUnit4])
class NullnessAnalyzerTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = NullnessAnalyzerTest
  val noOptCompiler = NullnessAnalyzerTest.noOptCompiler

  def newNullnessAnalyzer(methodNode: MethodNode, classInternalName: InternalName = "C"): NullnessAnalyzer = {
    val nullnessAnalyzer = new NullnessAnalyzer
    nullnessAnalyzer.analyze(classInternalName, methodNode)
    nullnessAnalyzer
  }

  def testNullness(analyzer: NullnessAnalyzer, method: MethodNode, query: String, index: Int, nullness: Nullness): Unit = {
    for (i <- findInstr(method, query)) {
      val r = analyzer.frameAt(i, method).getValue(index).nullness
      assertTrue(s"Expected: $nullness, found: $r. At instr ${textify(i)}", nullness == r)
    }
  }

  // debug / helper for writing tests
  def showAllNullnessFrames(analyzer: NullnessAnalyzer, method: MethodNode): String = {
    val instrLength = method.instructions.iterator.asScala.map(textify(_).length).max
    val lines = for (i <- method.instructions.iterator.asScala) yield {
      val f = analyzer.frameAt(i, method)
      val frameString = {
        if (f == null) "null"
        else (0 until (f.getLocals + f.getStackSize)).iterator
          .map(f.getValue(_).toString)
          .map(s => "%8s".format(s))
          .zipWithIndex.map({case (s, i) => s"$i: $s"})
          .mkString(", ")
      }
      ("%"+ instrLength +"s: %s").format(textify(i), frameString)
    }
    lines.mkString("\n")
  }

  @Test
  def showNullnessFramesTest(): Unit = {
    val List(m) = compileMethods(noOptCompiler)("def f = this.toString")

    // NOTE: the frame for an instruction represents the state *before* executing that instr.
    // So in the frame for `ALOAD 0`, the stack is still empty.

    val res =
      """                                                          L0: 0:  NotNull
        |                                             LINENUMBER 1 L0: 0:  NotNull
        |                                                     ALOAD 0: 0:  NotNull
        |INVOKEVIRTUAL java/lang/Object.toString ()Ljava/lang/String;: 0:  NotNull, 1:  NotNull
        |                                                     ARETURN: 0:  NotNull, 1: Unknown1
        |                                                          L0: null""".stripMargin
    assertEquals(showAllNullnessFrames(newNullnessAnalyzer(m), m), res)
  }

  @Test
  def thisNonNull(): Unit = {
    val List(m) = compileMethods(noOptCompiler)("def f = this.toString")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "ALOAD 0", 0, NotNull)
  }

  @Test
  def instanceMethodCall(): Unit = {
    val List(m) = compileMethods(noOptCompiler)("def f(a: String) = a.trim")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "INVOKEVIRTUAL java/lang/String.trim", 1, Unknown)
    testNullness(a, m, "ARETURN", 1, NotNull)
  }

  @Test
  def constructorCall(): Unit = {
    val List(m) = compileMethods(noOptCompiler)("def f = { val a = new Object; a.toString }")
    val a = newNullnessAnalyzer(m)

    // for reference, the output of showAllNullnessFrames(a, m) - note that the frame represents the state *before* executing the instr.
    //                    NEW java/lang/Object: 0: NotNull, 1: Unknown
    //                                     DUP: 0: NotNull, 1: Unknown, 2: Unknown
    //   INVOKESPECIAL java/lang/Object.<init>: 0: NotNull, 1: Unknown, 2: Unknown, 3: Unknown
    //                                ASTORE 1: 0: NotNull, 1: Unknown, 2: NotNull
    //                                 ALOAD 1: 0: NotNull, 1: NotNull
    // INVOKEVIRTUAL java/lang/Object.toString: 0: NotNull, 1: NotNull, 2: NotNull
    //                                 ARETURN: 0: NotNull, 1: NotNull, 2: Unknown

    for ((insn, index, nullness) <- List(
      ("+NEW", 2, Unknown),                                    // new value at slot 2 on the stack
      ("+DUP", 3, Unknown),
      ("+INVOKESPECIAL java/lang/Object", 2, NotNull),         // after calling the initializer on 3, the value at 2 becomes NotNull
      ("ASTORE 1", 1, Unknown),                                // before the ASTORE 1, nullness of the value in local 1 is Unknown
      ("+ASTORE 1", 1, NotNull),                               // after storing the value at 2 in local 1, the local 1 is NotNull
      ("+ALOAD 1", 2, NotNull),                                // loading the value 1 puts a NotNull value on the stack (at 2)
      ("+INVOKEVIRTUAL java/lang/Object.toString", 2, Unknown) // nullness of value returned by `toString` is Unknown
    )) testNullness(a, m, insn, index, nullness)
  }

  @Test
  def explicitNull(): Unit = {
    val List(m) = compileMethods(noOptCompiler)("def f = { var a: Object = null; a }")
    val a = newNullnessAnalyzer(m)
    for ((insn, index, nullness) <- List(
      ("+ACONST_NULL", 2, Null),
      ("+ASTORE 1", 1, Null),
      ("+ALOAD 1", 2, Null)
    )) testNullness(a, m, insn, index, nullness)
  }

  @Test
  def stringLiteralsNotNull(): Unit = {
    val List(m) = compileMethods(noOptCompiler)("""def f = { val a = "hi"; a.trim }""")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "+ASTORE 1", 1, NotNull)
  }

  @Test
  def newArraynotNull() {
    val List(m) = compileMethods(noOptCompiler)("def f = { val a = new Array[Int](2); a(0) }")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "+NEWARRAY T_INT", 2, NotNull) // new array on stack
    testNullness(a, m, "+ASTORE 1", 1, NotNull)       // local var (a)
  }

  @Test
  def aliasBranching(): Unit = {
    val code =
      """def f(o: Object) = {
        |  var a: Object = o     // a and o are aliases
        |  var b: Object = null
        |  var c: Object = null
        |  var d: Object = o
        |  if ("".trim == "") {
        |    b = o
        |    c = o               // a, o, b,  aliases
        |    d = null
        |  } else {
        |    b = a               // a, o, b aliases
        |    d = null
        |  }
        |  b.toString // a, o, b aliases (so they become NotNull), but not c
        |  // d is null here, assinged in both branches.
        |}
      """.stripMargin
    val List(m) = compileMethods(noOptCompiler)(code)
    val a = newNullnessAnalyzer(m)

    val trim = "INVOKEVIRTUAL java/lang/String.trim"
    val toSt = "INVOKEVIRTUAL java/lang/Object.toString"
    val end  = s"+$toSt"
    for ((insn, index, nullness) <- List(
      (trim, 0, NotNull), // this
      (trim, 1, Unknown), // parameter o
      (trim, 2, Unknown), // a
      (trim, 3, Null),    // b
      (trim, 4, Null),    // c
      (trim, 5, Unknown), // d

      (toSt, 2, Unknown), // a, still the same
      (toSt, 3, Unknown), // b, was re-assinged in both branches to Unknown
      (toSt, 4, Unknown), // c, was re-assigned in one branch to Unknown
      (toSt, 5, Null),    // d, was assigned to null in both branches

      (end, 2, NotNull),  // a, NotNull (alias of b)
      (end, 3, NotNull),  // b, receiver of toString
      (end, 4, Unknown),  // c, no change (not an alias of b)
      (end, 5, Null)      // d, no change
    )) testNullness(a, m, insn, index, nullness)
  }

  @Test
  def testInstanceOf(): Unit = {
    val code =
      """def f(a: Object) = {
        |  val x = a
        |  x.isInstanceOf[Throwable]    // x and a remain unknown - INSTANCEOF doesn't throw a NPE on null
        |  x.toString                   // x and a are not null
        |  a.asInstanceOf[String].trim  // the stack value (LOAD of local a) is still not-null after the CHECKCAST
        |}
      """.stripMargin
    val List(m) = compileMethods(noOptCompiler)(code)
    val a = newNullnessAnalyzer(m)

    val instof = "+INSTANCEOF"
    val tost   = "+INVOKEVIRTUAL java/lang/Object.toString"
    val trim   = "INVOKEVIRTUAL java/lang/String.trim"

    for ((insn, index, nullness) <- List(
      (instof, 1, Unknown), // a after INSTANCEOF
      (instof, 2, Unknown), // x after INSTANCEOF
      (tost, 1, NotNull),
      (tost, 2, NotNull),
      (trim, 3, NotNull)  // receiver at `trim`
    )) testNullness(a, m, insn, index, nullness)
  }
}

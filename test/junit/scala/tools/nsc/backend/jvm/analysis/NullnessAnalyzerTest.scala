package scala.tools.nsc
package backend.jvm
package analysis

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.asm.tree.MethodNode
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.nsc.backend.jvm.BTypes._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class NullnessAnalyzerTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler._
  import global.genBCode.bTypes.backendUtils._

  def newNullnessAnalyzer(methodNode: MethodNode, classInternalName: InternalName = "C") = new AsmAnalyzer(methodNode, classInternalName, new NullnessAnalyzer(global.genBCode.bTypes, methodNode))

  def testNullness(analyzer: AsmAnalyzer[NullnessValue], method: MethodNode, query: String, index: Int, nullness: NullnessValue): Unit = {
    for (i <- findInstrs(method, query)) {
      val r = analyzer.frameAt(i).getValue(index)
      assertTrue(s"Expected: $nullness, found: $r. At instr ${textify(i)}", nullness == r)
    }
  }

  // debug / helper for writing tests
  def showAllNullnessFrames(analyzer: AsmAnalyzer[NullnessValue], method: MethodNode): String = {
    val instrLength = method.instructions.iterator.asScala.map(textify(_).length).max
    val lines = for (i <- method.instructions.iterator.asScala) yield {
      val f = analyzer.frameAt(i)
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
    val m = compileAsmMethod("def f = this.toString")

    // NOTE: the frame for an instruction represents the state *before* executing that instr.
    // So in the frame for `ALOAD 0`, the stack is still empty.

    val res =
      """                                           L0: 0:  NotNull
        |                              LINENUMBER 1 L0: 0:  NotNull
        |                                      ALOAD 0: 0:  NotNull
        |INVOKEVIRTUAL C.toString ()Ljava/lang/String;: 0:  NotNull, 1:  NotNull
        |                                      ARETURN: 0:  NotNull, 1: Unknown1
        |                                           L0: null""".stripMargin
//    println(showAllNullnessFrames(newNullnessAnalyzer(m), m))
    assertEquals(showAllNullnessFrames(newNullnessAnalyzer(m), m), res)
  }

  @Test
  def thisNonNull(): Unit = {
    val m = compileAsmMethod("def f = this.toString")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "ALOAD 0", 0, NotNullValue)
  }

  @Test
  def instanceMethodCall(): Unit = {
    val m = compileAsmMethod("def f(a: String) = a.trim")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "INVOKEVIRTUAL java/lang/String.trim", 1, UnknownValue1)
    testNullness(a, m, "ARETURN", 1, NotNullValue)
  }

  @Test
  def constructorCall(): Unit = {
    val m = compileAsmMethod("def f = { val a = new Object; a.toString }")
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
      ("+NEW", 2, UnknownValue1),                                    // new value at slot 2 on the stack
      ("+DUP", 3, UnknownValue1),
      ("+INVOKESPECIAL java/lang/Object", 2, NotNullValue),          // after calling the initializer on 3, the value at 2 becomes NotNull
      ("ASTORE 1", 1, UnknownValue1),                                // before the ASTORE 1, nullness of the value in local 1 is Unknown
      ("+ASTORE 1", 1, NotNullValue),                                // after storing the value at 2 in local 1, the local 1 is NotNull
      ("+ALOAD 1", 2, NotNullValue),                                 // loading the value 1 puts a NotNull value on the stack (at 2)
      ("+INVOKEVIRTUAL java/lang/Object.toString", 2, UnknownValue1) // nullness of value returned by `toString` is Unknown
    )) testNullness(a, m, insn, index, nullness)
  }

  @Test
  def explicitNull(): Unit = {
    val m = compileAsmMethod("def f = { var a: Object = null; a }")
    val a = newNullnessAnalyzer(m)
    for ((insn, index, nullness) <- List(
      ("+ACONST_NULL", 2, NullValue),
      ("+ASTORE 1", 1, NullValue),
      ("+ALOAD 1", 2, NullValue)
    )) testNullness(a, m, insn, index, nullness)
  }

  @Test
  def stringLiteralsNotNull(): Unit = {
    val m = compileAsmMethod("""def f = { val a = "hi"; a.trim }""")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "+ASTORE 1", 1, NotNullValue)
  }

  @Test
  def newArraynotNull() {
    val m = compileAsmMethod("def f = { val a = new Array[Int](2); a(0) }")
    val a = newNullnessAnalyzer(m)
    testNullness(a, m, "+NEWARRAY T_INT", 2, NotNullValue) // new array on stack
    testNullness(a, m, "+ASTORE 1", 1, NotNullValue)       // local var (a)
  }

  @Test
  def mergeNullNotNull(): Unit = {
    val code =
      """def f(o: Object) = {
        |  var a: Object = o
        |  var c: Object = null
        |  if ("".trim eq "-") {
        |    c = o
        |  }
        |  a.toString
        |}
      """.stripMargin
    val m = compileAsmMethod(code)
    val a = newNullnessAnalyzer(m)
    val toSt = "+INVOKEVIRTUAL java/lang/Object.toString"
    testNullness(a, m, toSt, 3, UnknownValue1)
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
    val m = compileAsmMethod(code)
    val a = newNullnessAnalyzer(m)

    val trim = "INVOKEVIRTUAL java/lang/String.trim"
    val toSt = "INVOKEVIRTUAL java/lang/Object.toString"
    val end  = s"+$toSt"
    for ((insn, index, nullness) <- List(
      (trim, 0, NotNullValue),  // this
      (trim, 1, UnknownValue1), // parameter o
      (trim, 2, UnknownValue1), // a
      (trim, 3, NullValue),     // b
      (trim, 4, NullValue),     // c
      (trim, 5, UnknownValue1), // d

      (toSt, 2, UnknownValue1), // a, still the same
      (toSt, 3, UnknownValue1), // b, was re-assinged in both branches to Unknown
      (toSt, 4, UnknownValue1), // c, was re-assigned in one branch to Unknown
      (toSt, 5, NullValue),     // d, was assigned to null in both branches

      (end, 2, NotNullValue),   // a, NotNull (alias of b)
      (end, 3, NotNullValue),   // b, receiver of toString
      (end, 4, UnknownValue1),  // c, no change (not an alias of b)
      (end, 5, NullValue)       // d, no change
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
    val m = compileAsmMethod(code)
    val a = newNullnessAnalyzer(m)

    val instof = "+INSTANCEOF"
    val tost   = "+INVOKEVIRTUAL java/lang/Object.toString"
    val trim   = "INVOKEVIRTUAL java/lang/String.trim"

    for ((insn, index, nullness) <- List(
      (instof, 1, UnknownValue1), // a after INSTANCEOF
      (instof, 2, UnknownValue1), // x after INSTANCEOF
      (tost, 1, NotNullValue),
      (tost, 2, NotNullValue),
      (trim, 3, NotNullValue)  // receiver at `trim`
    )) testNullness(a, m, insn, index, nullness)
  }
}

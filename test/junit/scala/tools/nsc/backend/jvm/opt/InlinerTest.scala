package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.generic.Clearable
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils.BasicAnalyzer
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._

import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object InlinerTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:l:classpath")

  // allows inspecting the caches after a compilation run
  def notPerRun: List[Clearable] = List(compiler.genBCode.bTypes.classBTypeFromInternalName, compiler.genBCode.bTypes.byteCodeRepository.classes, compiler.genBCode.bTypes.callGraph.callsites)
  notPerRun foreach compiler.perRunCaches.unrecordCache

  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class InlinerTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = InlinerTest

  val compiler = InlinerTest.compiler
  import compiler.genBCode.bTypes._

  def compile(code: String): List[ClassNode] = {
    InlinerTest.notPerRun.foreach(_.clear())
    compileClasses(compiler)(code)
  }

  def checkCallsite(callsite: callGraph.Callsite, callee: MethodNode) = {
    assert(callsite.callsiteMethod.instructions.contains(callsite.callsiteInstruction), instructionsFromMethod(callsite.callsiteMethod))

    val callsiteClassNode = byteCodeRepository.classNode(callsite.callsiteClass.internalName)
    assert(callsiteClassNode.methods.contains(callsite.callsiteMethod), callsiteClassNode.methods.asScala.map(_.name).toList)

    assert(callsite.callee.get.callee == callee, callsite.callee.get.callee.name)
  }

  // inline first invocation of f into g in class C
  def inlineTest(code: String, mod: ClassNode => Unit = _ => ()): (MethodNode, Option[String]) = {
    val List(cls) = compile(code)
    mod(cls)
    val clsBType = classBTypeFromParsedClassfile(cls.name)

    val List(f, g) = cls.methods.asScala.filter(m => Set("f", "g")(m.name)).toList.sortBy(_.name)
    val fCall = g.instructions.iterator.asScala.collect({ case i: MethodInsnNode if i.name == "f" => i }).next()

    val analyzer = new BasicAnalyzer(g, clsBType.internalName)

    val r = inliner.inline(
      fCall,
      analyzer.frameAt(fCall).getStackSize,
      g,
      clsBType,
      f,
      clsBType,
      receiverKnownNotNull = true,
      keepLineNumbers = true)
    (g, r)
  }

  @Test
  def simpleInlineOK(): Unit = {
    val code =
      """class C {
        |  def f = 1
        |  def g = f + f
        |}
      """.stripMargin

    val (g, _) = inlineTest(code)

    val gConv = convertMethod(g)
    assertSameCode(gConv.instructions.dropNonOp,
      List(
        VarOp(ALOAD, 0), VarOp(ASTORE, 1), // store this
        Op(ICONST_1), VarOp(ISTORE, 2), Jump(GOTO, Label(10)), // store return value
        Label(10), VarOp(ILOAD, 2), // load return value
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "f", "()I", false), Op(IADD), Op(IRETURN)))

    // line numbers are kept, so there's a line 2 (from the inlined f)
    assert(gConv.instructions exists {
      case LineNumber(2, _) => true
      case _ => false
    }, gConv.instructions.filter(_.isInstanceOf[LineNumber]))

    assert(gConv.localVars.map(_.name).sorted == List("f_this", "this"), gConv.localVars)
    assert(g.maxStack == 2 && g.maxLocals == 3, s"${g.maxLocals} - ${g.maxStack}")
  }

  @Test
  def nothingTypedOK(): Unit = {
    val code =
      """class C {
        |  def f: Nothing = ???
        |  def g: Int = { f; 1 }
        |}
      """.stripMargin

    // On the bytecode level, methods of type Nothing have return type Nothing$.
    // This can be treated like any other result object.

    // See also discussion around ATHROW in BCodeBodyBuilder

    val (g, _) = inlineTest(code)
    val expectedInlined = List(
      VarOp(ALOAD, 0), VarOp(ASTORE, 1), // store this
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"), Invoke(INVOKEVIRTUAL, "scala/Predef$", "$qmark$qmark$qmark", "()Lscala/runtime/Nothing$;", false)) // inlined call to ???

    assertSameCode(convertMethod(g).instructions.dropNonOp.take(4), expectedInlined)

    localOpt.methodOptimizations(g, "C")
    assertSameCode(convertMethod(g).instructions.dropNonOp,
      expectedInlined ++ List(VarOp(ASTORE, 2), VarOp(ALOAD, 2), Op(ATHROW)))
  }

  @Test
  def synchronizedNoInline(): Unit = {
    val code =
      """class C {
        |  def f: Int = 0
        |  def g: Int = f
        |}
      """.stripMargin

    val (_, can) = inlineTest(code, cls => {
      val f = cls.methods.asScala.find(_.name == "f").get
      f.access |= ACC_SYNCHRONIZED
    })
    assert(can.get contains "synchronized", can)
  }

  @Test
  def tryCatchOK(): Unit = {
    val code =
      """class C {
        |  def f: Int = try { 1 } catch { case _: Exception => 2 }
        |  def g = f + 1
        |}
      """.stripMargin
    val (_, r) = inlineTest(code)
    assert(r.isEmpty, r)
  }

  @Test
  def tryCatchNoInline(): Unit = {
    // cannot inline f: there's a value on g's stack. if f throws and enters the handler, all values
    // on the stack are removed, including the one of g's stack that we still need.
    val code =
      """class C {
        |  def f: Int = try { 1 } catch { case _: Exception => 2 }
        |  def g = println(f)
        |}
      """.stripMargin
    val (_, r) = inlineTest(code)
    assert(r.get contains "operand stack at the callsite", r)
  }

  @Test
  def illegalAccessNoInline(): Unit = {
    val code =
      """package a {
        |  class C {
        |    private def f: Int = 0
        |    def g: Int = f
        |  }
        |}
        |package b {
        |  class D {
        |    def h(c: a.C): Int = c.g + 1
        |  }
        |}
      """.stripMargin

    val List(c, d) = compile(code)

    val cTp = classBTypeFromParsedClassfile(c.name)
    val dTp = classBTypeFromParsedClassfile(d.name)

    val g = c.methods.asScala.find(_.name == "g").get
    val h = d.methods.asScala.find(_.name == "h").get
    val gCall = h.instructions.iterator.asScala.collect({
      case m: MethodInsnNode if m.name == "g" => m
    }).next()

    val analyzer = new BasicAnalyzer(h, dTp.internalName)

    val r = inliner.inline(
      gCall,
      analyzer.frameAt(gCall).getStackSize,
      h,
      dTp,
      g,
      cTp,
      receiverKnownNotNull = true,
      keepLineNumbers = true)

    assert(r.get contains "would cause an IllegalAccessError", r)
  }

  @Test
  def inlineSimpleAtInline(): Unit = {
    val code =
      """class C {
        |  @inline final def f = 0
        |  final def g = 1
        |
        |  def test = f + g
        |}
      """.stripMargin
    val List(cCls) = compile(code)
    val instructions = instructionsFromMethod(cCls.methods.asScala.find(_.name == "test").get)
    assert(instructions.contains(Op(ICONST_0)), instructions mkString "\n")
    assert(!instructions.contains(Op(ICONST_1)), instructions)
  }

  @Test
  def cyclicInline(): Unit = {
    val code =
      """class C {
        |  @inline final def f: Int = g
        |  @inline final def g: Int = f
        |}
      """.stripMargin
    val List(c) = compile(code)
    val methods @ List(_, g) = c.methods.asScala.filter(_.name.length == 1).toList
    val List(fIns, gIns) = methods.map(instructionsFromMethod(_).dropNonOp)
    val invokeG = Invoke(INVOKEVIRTUAL, "C", "g", "()I", false)
    assert(fIns contains invokeG, fIns) // no inlining into f, that request is elided
    assert(gIns contains invokeG, gIns) // f is inlined into g, g invokes itself recursively

    assert(callGraph.callsites.size == 3, callGraph.callsites)
    for (callsite <- callGraph.callsites.values if methods.contains(callsite.callsiteMethod)) {
      checkCallsite(callsite, g)
    }
  }

  @Test
  def cyclicInline2(): Unit = {
    val code =
      """class C {
        |  @inline final def h: Int = f
        |  @inline final def f: Int = g + g
        |  @inline final def g: Int = h
        |}
      """.stripMargin
    val List(c) = compile(code)
    val methods @ List(f, g, h) = c.methods.asScala.filter(_.name.length == 1).sortBy(_.name).toList
    val List(fIns, gIns, hIns) = methods.map(instructionsFromMethod(_).dropNonOp)
    val invokeG = Invoke(INVOKEVIRTUAL, "C", "g", "()I", false)
    assert(fIns.count(_ == invokeG) == 2, fIns) // no inlining into f, these requests are elided
    assert(gIns.count(_ == invokeG) == 2, gIns)
    assert(hIns.count(_ == invokeG) == 2, hIns)

    assert(callGraph.callsites.size == 7, callGraph.callsites)
    for (callsite <- callGraph.callsites.values if methods.contains(callsite.callsiteMethod)) {
      checkCallsite(callsite, g)
    }
  }

  @Test
  def arraycopy(): Unit = {
    // also tests inlining of a void-returning method (no return value on the stack)
    val code =
      """class C {
        |  def f(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = {
        |    compat.Platform.arraycopy(src, srcPos, dest, destPos, length)
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)
    val ins = instructionsFromMethod(c.methods.asScala.find(_.name == "f").get)
    val invokeSysArraycopy = Invoke(INVOKESTATIC, "java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", false)
    assert(ins contains invokeSysArraycopy, ins mkString "\n")
  }

  @Test
  def arrayMemberMethod(): Unit = {
    // This used to crash when building the call graph. The `owner` field of the MethodInsnNode
    // for the invocation of `clone` is not an internal name, but a full array descriptor
    // [Ljava.lang.Object; - the documentation in the ASM library didn't mention that possibility.
    val code =
      """class C {
        |  def f(a: Array[Object]) = {
        |    a.clone()
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)
    assert(callGraph.callsites.values exists (_.callsiteInstruction.name == "clone"))
  }

  @Test
  def atInlineInTraitDoesNotCrash(): Unit = {
    val code =
      """trait T {
        |  @inline final def f = 0
        |}
        |class C {
        |  def g(t: T) = t.f
        |}
      """.stripMargin
    val List(c, t, tClass) = compile(code)
    val ins = instructionsFromMethod(c.methods.asScala.find(_.name == "g").get)
    val invokeF = Invoke(INVOKEINTERFACE, "T", "f", "()I", true)
    // no inlining yet
    assert(ins contains invokeF, ins mkString "\n")
  }

  @Test
  def inlinePrivateMethodWithHandler(): Unit = {
    val code =
      """class C {
        |  @inline private def f = try { 0 } catch { case _: Throwable => 1 }
        |  def g = f
        |}
      """.stripMargin
    val List(c) = compile(code)
    val ins = instructionsFromMethod(c.methods.asScala.find(_.name == "g").get)
    println(ins)
    // no more invoke, f is inlined
    assert(ins.count(_.isInstanceOf[Invoke]) == 0, ins mkString "\n")
  }

  @Test
  def inlineStaticCall(): Unit = {
    val code =
      """class C {
        |  def f = Integer.lowestOneBit(103)
        |}
      """.stripMargin

    val List(c) = compile(code)
    val f = c.methods.asScala.find(_.name == "f").get
    val callsiteIns = f.instructions.iterator().asScala.collect({ case c: MethodInsnNode => c }).next()
    val clsBType = classBTypeFromParsedClassfile(c.name)
    val analyzer = new BasicAnalyzer(f, clsBType.internalName)

    val integerClassBType = classBTypeFromInternalName("java/lang/Integer")
    val lowestOneBitMethod = byteCodeRepository.methodNode(integerClassBType.internalName, "lowestOneBit", "(I)I").get._1

    val r = inliner.inline(
      callsiteIns,
      analyzer.frameAt(callsiteIns).getStackSize,
      f,
      clsBType,
      lowestOneBitMethod,
      integerClassBType,
      receiverKnownNotNull = false,
      keepLineNumbers = false)

    assert(r.isEmpty, r)
    val ins = instructionsFromMethod(f)

    // no invocations, lowestOneBit is inlined
    assert(ins.count(_.isInstanceOf[Invoke]) == 0, ins mkString "\n")

    // no null check when inlining a static method
    ins foreach {
      case Jump(IFNONNULL, _) => assert(false, ins mkString "\n")
      case _ =>
    }
  }

  @Test
  def maxLocalsMaxStackAfterInline(): Unit = {
    val code =
      """class C {
        |  @inline final def f1(x: Int): Int = {
        |    val a = x + 1
        |    math.max(a, math.min(10, a - 1))
        |  }
        |
        |  @inline final def f2(x: Int): Unit = {
        |    val a = x + 1
        |    println(math.max(a, 10))
        |  }
        |
        |  def g1 = println(f1(32))
        |  def g2 = println(f2(32))
        |}
      """.stripMargin

    val List(c) = compile(code)
    val ms @ List(f1, f2, g1, g2) = c.methods.asScala.filter(_.name.length == 2).toList

    // stack height at callsite of f1 is 1, so max of g1 after inlining is max of f1 + 1
    assert(g1.maxStack == 7 && f1.maxStack == 6, s"${g1.maxStack} - ${f1.maxStack}")

    // locals in f1: this, x, a
    // locals in g1 after inlining: this, this-of-f1, x, a, return value
    assert(g1.maxLocals == 5 && f1.maxLocals == 3, s"${g1.maxLocals} - ${f1.maxLocals}")

    // like maxStack in g1 / f1
    assert(g2.maxStack == 5 && f2.maxStack == 4, s"${g2.maxStack} - ${f2.maxStack}")

    // like maxLocals for g1 / f1, but no return value
    assert(g2.maxLocals == 4 && f2.maxLocals == 3, s"${g2.maxLocals} - ${f2.maxLocals}")
  }
}

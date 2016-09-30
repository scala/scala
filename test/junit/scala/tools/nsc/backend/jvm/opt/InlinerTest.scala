package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.collection.generic.Clearable
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class InlinerTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:classpath -opt-warnings"

  val inlineOnlyCompiler = cached("inlineOnlyCompiler", () => newCompiler(extraArgs = "-opt:inline-project"))

  import compiler._
  import global.genBCode.bTypes
  // allows inspecting the caches after a compilation run
  def notPerRun: List[Clearable] = List(
    bTypes.classBTypeFromInternalName,
    bTypes.byteCodeRepository.compilingClasses,
    bTypes.byteCodeRepository.parsedClasses,
    bTypes.callGraph.callsites)
  notPerRun foreach global.perRunCaches.unrecordCache

  import global.genBCode.bTypes.{byteCodeRepository, callGraph, inliner, inlinerHeuristics}
  import inlinerHeuristics._


  def compile(scalaCode: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): List[ClassNode] = {
    notPerRun.foreach(_.clear())
    compileToBytes(scalaCode, javaCode, allowMessage)
    // Use the class nodes stored in the byteCodeRepository. The ones returned by compileClasses are not the same,
    // these are created new from the classfile byte array. They are completely separate instances which cannot
    // be used to look up methods / callsites in the callGraph hash maps for example.
    byteCodeRepository.compilingClasses.valuesIterator.map(_._1).toList.sortBy(_.name)
  }

  def checkCallsite(callsite: callGraph.Callsite, callee: MethodNode) = {
    assert(callsite.callsiteMethod.instructions.contains(callsite.callsiteInstruction), instructionsFromMethod(callsite.callsiteMethod))

    val callsiteClassNode = byteCodeRepository.classNode(callsite.callsiteClass.internalName).get
    assert(callsiteClassNode.methods.contains(callsite.callsiteMethod), callsiteClassNode.methods.asScala.map(_.name).toList)

    assert(callsite.callee.get.callee == callee, callsite.callee.get.callee.name)
  }

  def getCallsite(method: MethodNode, calleeName: String) = callGraph.callsites(method).valuesIterator.find(_.callee.get.callee.name == calleeName).get

  def gMethAndFCallsite(code: String, mod: ClassNode => Unit = _ => ()) = {
    val List(c) = compile(code)
    mod(c)
    val gMethod = getAsmMethod(c, "g")
    val fCall = getCallsite(gMethod, "f")
    (gMethod, fCall)
  }

  def canInlineTest(code: String, mod: ClassNode => Unit = _ => ()): Option[OptimizerWarning] = {
    val cs = gMethAndFCallsite(code, mod)._2
    inliner.earlyCanInlineCheck(cs) orElse inliner.canInlineBody(cs)
  }

  def inlineTest(code: String, mod: ClassNode => Unit = _ => ()): MethodNode = {
    val (gMethod, fCall) = gMethAndFCallsite(code, mod)
    inliner.inline(InlineRequest(fCall, Nil, null))
    gMethod
  }

  @Test
  def simpleInlineOK(): Unit = {
    val code =
      """class C {
        |  def f = 1
        |  def g = f + f
        |}
      """.stripMargin

    val g = inlineTest(code)

    val gConv = convertMethod(g)
    assertSameCode(gConv,
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

    val g = inlineTest(code)

    val invokeQQQ = List(
      Field(GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
      Invoke(INVOKEVIRTUAL, "scala/Predef$", "$qmark$qmark$qmark", "()Lscala/runtime/Nothing$;", false))

    val gBeforeLocalOpt = VarOp(ALOAD, 0) :: VarOp(ASTORE, 1) :: invokeQQQ ::: List(
      VarOp(ASTORE, 2),
      Jump(GOTO, Label(11)),
      Label(11),
      VarOp(ALOAD, 2),
      Op(ATHROW))

    assertSameCode(convertMethod(g), gBeforeLocalOpt)

    global.genBCode.bTypes.localOpt.methodOptimizations(g, "C")
    assertSameCode(convertMethod(g), invokeQQQ :+ Op(ATHROW))
  }

  @Test
  def synchronizedNoInline(): Unit = {
    val code =
      """class C {
        |  def f: Int = 0
        |  def g: Int = f
        |}
      """.stripMargin

    val can = canInlineTest(code, cls => {
      val f = getAsmMethod(cls, "f")
      f.access |= ACC_SYNCHRONIZED
    })
    assert(can.nonEmpty && can.get.isInstanceOf[SynchronizedMethod], can)
  }

  @Test
  def tryCatchOK(): Unit = {
    val code =
      """class C {
        |  def f: Int = try { 1 } catch { case _: Exception => 2 }
        |  def g = f + 1
        |}
      """.stripMargin
    val r = canInlineTest(code)
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
    val r = canInlineTest(code)
    assert(r.nonEmpty && r.get.isInstanceOf[MethodWithHandlerCalledOnNonEmptyStack], r)
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
    val hMeth = getAsmMethod(d, "h")
    val gCall = getCallsite(hMeth, "g")
    val r = inliner.canInlineBody(gCall)
    assert(r.nonEmpty && r.get.isInstanceOf[IllegalAccessInstruction], r)
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
    val instructions = getInstructions(cCls, "test")
    assert(instructions.contains(Op(ICONST_0)), instructions.stringLines)
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
    for (callsite <- callGraph.callsites.valuesIterator.flatMap(_.valuesIterator) if methods.contains(callsite.callsiteMethod)) {
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

    assert(callGraph.callsites.valuesIterator.flatMap(_.valuesIterator).size == 7, callGraph.callsites)
    for (callsite <- callGraph.callsites.valuesIterator.flatMap(_.valuesIterator) if methods.contains(callsite.callsiteMethod)) {
      checkCallsite(callsite, g)
    }
  }

  @Test
  def arraycopy(): Unit = {
    // also tests inlining of a void-returning method (no return value on the stack)
    val code =
      """// can't use the `compat.Platform.arraycopy` from the std lib for now, because the classfile doesn't have a ScalaInlineInfo attribute
        |object Platform {
        |  @inline def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int) {
        |    System.arraycopy(src, srcPos, dest, destPos, length)
        |  }
        |}
        |class C {
        |  def f(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = {
        |    Platform.arraycopy(src, srcPos, dest, destPos, length)
        |  }
        |}
      """.stripMargin
    val List(c, _, _) = compile(code)
    val ins = getInstructions(c, "f")
    val invokeSysArraycopy = Invoke(INVOKESTATIC, "java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", false)
    assert(ins contains invokeSysArraycopy, ins.stringLines)
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
    assert(callGraph.callsites.valuesIterator.flatMap(_.valuesIterator) exists (_.callsiteInstruction.name == "clone"))
  }

  @Test
  def atInlineInTrait(): Unit = {
    val code =
      """trait T {
        |  @inline final def f = 0
        |}
        |class C {
        |  def g(t: T) = t.f
        |}
      """.stripMargin
    val List(c, t) = compile(code)
    assertNoInvoke(getMethod(c, "g"))
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
    // no more invoke, f is inlined
    assertNoInvoke(getMethod(c, "g"))
  }

  @Test
  def inlineStaticCall(): Unit = {
    val code =
      """class C {
        |  def f = Integer.lowestOneBit(103)
        |}
      """.stripMargin

    val List(c) = compile(code)
    val fMeth = getAsmMethod(c, "f")
    val call = getCallsite(fMeth, "lowestOneBit")

    val warning = inliner.canInlineBody(call)
    assert(warning.isEmpty, warning)

    inliner.inline(InlineRequest(call, Nil, null))
    val ins = instructionsFromMethod(fMeth)

    // no invocations, lowestOneBit is inlined
    assertNoInvoke(ins)

    // no null check when inlining a static method
    ins foreach {
      case Jump(IFNONNULL, _) => assert(false, ins.stringLines)
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

    // use a compiler without local optimizations (cleanups)
    val c = inlineOnlyCompiler.compileClass(code)
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

  @Test
  def mixedCompilationNoInline(): Unit = {
    // The inliner checks if the invocation `A.bar` can be safely inlined. For that it needs to have
    // the bytecode of the invoked method. In mixed compilation, there's no classfile available for
    // A, so `flop` cannot be inlined, we cannot check if it's safe.

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

    val warn =
      """B::flop()I is annotated @inline but could not be inlined:
        |Failed to check if B::flop()I can be safely inlined to B without causing an IllegalAccessError. Checking instruction INVOKESTATIC A.bar ()I failed:
        |The method bar()I could not be found in the class A or any of its parents.
        |Note that class A is defined in a Java source (mixed compilation), no bytecode is available.""".stripMargin

    var c = 0
    val List(b) = compile(scalaCode, List((javaCode, "A.java")), allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)
    val ins = getInstructions(b, "g")
    val invokeFlop = Invoke(INVOKEVIRTUAL, "B", "flop", "()I", false)
    assert(ins contains invokeFlop, ins.stringLines)
  }

  @Test
  def inlineFromTraits(): Unit = {
    val code =
      """trait T {
        |  @inline final def f = g
        |  @inline final def g = 1
        |}
        |
        |class C extends T {
        |  def t1(t: T) = t.f
        |  def t2(c: C) = c.f
        |}
      """.stripMargin
    val List(c, t) = compile(code)
    // both are just `return 1`, no more calls
    assertNoInvoke(getMethod(c, "t1"))
    assertNoInvoke(getMethod(c, "t2"))
  }

  @Test
  def inlineTraitInherited(): Unit = {
    val code =
      """trait T {
        |  @inline final def f = 1
        |}
        |trait U extends T {
        |  @inline final def g = f
        |}
        |class C extends U {
        |  def t1 = f
        |  def t2 = g
        |}
      """.stripMargin
    val List(c, t, u) = compile(code)
    assertNoInvoke(getMethod(c, "t1"))
    assertNoInvoke(getMethod(c, "t2"))
  }

  @Test
  def virtualTraitNoInline(): Unit = {
    val code =
      """trait T {
        |  @inline def f = 1
        |}
        |class C extends T {
        |  def t1(t: T) = t.f
        |  def t2 = this.f
        |}
      """.stripMargin
    val warn = "::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden"
    var count = 0
    val List(c, t) = compile(code, allowMessage = i => {count += 1; i.msg contains warn})
    assert(count == 2, count)
    assertInvoke(getMethod(c, "t1"), "T", "f")
    assertInvoke(getMethod(c, "t2"), "C", "f")
  }

  @Test
  def sealedTraitInline(): Unit = {
    val code =
      """sealed trait T {
        |  @inline def f = 1
        |}
        |class C {
        |  def t1(t: T) = t.f
        |}
      """.stripMargin
    val List(c, t) = compile(code)
    assertNoInvoke(getMethod(c, "t1"))
  }

  @Test
  def inlineFromObject(): Unit = {
    val code =
      """trait T {
        |  @inline def f = 0
        |}
        |object O extends T {
        |  @inline def g = 1
        |  // mixin generates `def f = super[T].f`, which is inlined here (we get ICONST_0)
        |}
        |class C {
        |  def t1 = O.f       // the mixin method of O is inlined, so we directly get the ICONST_0
        |  def t2 = O.g       // object members are inlined
        |  def t3(t: T) = t.f // no inlining here
        |}
      """.stripMargin
    val warn = "T::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden"
    var count = 0
    val List(c, oMirror, oModule, t) = compile(code, allowMessage = i => {count += 1; i.msg contains warn})
    assert(count == 1, count)

    assertNoInvoke(getMethod(t, "f"))

    assertNoInvoke(getMethod(c, "t1"))
    assertNoInvoke(getMethod(c, "t2"))
    assertInvoke(getMethod(c, "t3"), "T", "f")
  }

  @Test
  def selfTypeInline(): Unit = {
    val code =
      """trait T { self: Assembly =>
        |  @inline final def f = g
        |  @inline final def m = 1
        |}
        |trait Assembly extends T {
        |  @inline final def g = 1
        |  @inline final def n = m // inlined (m is final)
        |}
        |class C {
        |  def t1(a: Assembly) = a.f // inlined (f is final)
        |  def t2(a: Assembly) = a.n
        |}
      """.stripMargin

    val List(assembly, c, t) = compile(code)

    assertNoInvoke(getMethod(t, "f"))

    assertNoInvoke(getMethod(assembly, "n"))

    assertNoInvoke(getMethod(c, "t1"))
    assertNoInvoke(getMethod(c, "t2"))
  }

  @Test
  def selfTypeInline2(): Unit = {
    // There are some interesting things going on here with the self types. Here's a short version:
    //
    //   trait T1 { def f = 1 }
    //   trait T2a { self: T1 with T2a =>  // self type in the backend: T1
    //     def f = 2
    //     def g = f                       // resolved to T2a.f
    //   }
    //   trait T2b { self: T2b with T1 =>  // self type in the backend: T2b
    //     def f = 2
    //     def g = f                       // resolved to T1.f
    //   }
    //
    // scala> val t = typeOf[T2a]; exitingMixin(t.typeOfThis.typeSymbol)  // self type of T2a is T1
    // res28: $r.intp.global.Symbol = trait T1
    //
    // scala> typeOf[T2a].typeOfThis.member(newTermName("f")).owner       // f in T2a is resolved as T2a.f
    // res29: $r.intp.global.Symbol = trait T2a
    //
    // scala> val t = typeOf[T2b]; exitingMixin(t.typeOfThis.typeSymbol)  // self type of T2b is T1
    // res30: $r.intp.global.Symbol = trait T2b
    //
    // scala> typeOf[T2b].typeOfThis.member(newTermName("f")).owner       // f in T2b is resolved as T1.f
    // res31: $r.intp.global.Symbol = trait T1

    val code =
      """trait T1 {
        |  @inline def f: Int = 0
        |  @inline def g1 = f     // not inlined: f not final
        |}
        |
        |// erased self-type: T1
        |trait T2a { self: T1 with T2a =>
        |  @inline override final def f = 1
        |  @inline def g2a = f    // inlined: resolved as T2a.f
        |}
        |
        |final class Ca extends T1 with T2a {
        |  // mixin generates accessors like `def g1 = super[T1].g1`, the impl super call is inlined into the accessor.
        |
        |  def m1a = g1           // call to accessor, inlined, we get the interface call T1.f
        |  def m2a = g2a          // call to accessor, inlined, we get ICONST_1
        |  def m3a = f            // call to accessor, inlined, we get ICONST_1
        |
        |  def m4a(t: T1) = t.f   // T1.f is not final, so not inlined, we get an interface call T1.f
        |  def m5a(t: T2a) = t.f  // inlined, we get ICONST_1
        |}
        |
        |// erased self-type: T2b
        |trait T2b { self: T2b with T1 =>
        |  @inline override final def f = 1
        |  @inline def g2b = f    // not inlined: resolved as T1.f, we get an interface call T1.f
        |}
        |
        |final class Cb extends T1 with T2b {
        |  def m1b = g1           // inlined, we get the interface call to T1.f
        |  def m2b = g2b          // inlined, we get the interface call to T1.f
        |  def m3b = f            // inlined, we get ICONST_1
        |
        |  def m4b(t: T1) = t.f   // T1.f is not final, so not inlined, interface call to T1.f
        |  def m5b(t: T2b) = t.f  // inlined, ICONST_1
        |}
      """.stripMargin

    val warning = "T1::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden"
    var count = 0
    val List(ca, cb, t1, t2a, t2b) = compile(code, allowMessage = i => {count += 1; i.msg contains warning})
    assert(count == 4, count) // see comments, f is not inlined 4 times

    assertNoInvoke(getMethod(t2a, "g2a"))
    assertInvoke(getMethod(t2b, "g2b"), "T1", "f")

    assertInvoke(getMethod(ca, "m1a"), "T1", "f")
    assertNoInvoke(getMethod(ca, "m2a"))            // no invoke, see comment on def g2a
    assertNoInvoke(getMethod(ca, "m3a"))
    assertInvoke(getMethod(ca, "m4a"), "T1", "f")
    assertNoInvoke(getMethod(ca, "m5a"))

    assertInvoke(getMethod(cb, "m1b"), "T1", "f")
    assertInvoke(getMethod(cb, "m2b"), "T1", "f")  // invoke, see comment on def g2b
    assertNoInvoke(getMethod(cb, "m3b"))
    assertInvoke(getMethod(cb, "m4b"), "T1", "f")
    assertNoInvoke(getMethod(cb, "m5b"))
  }

  @Test
  def finalSubclassInline(): Unit = {
    val code =
      """class C {
        |  @inline def f = 0
        |  @inline final def g = 1
        |}
        |final class D extends C
        |object E extends C
        |class T {
        |  def t1(d: D) = d.f + d.g + E.f + E.g // d.f can be inlined because the receiver type is D, which is final.
        |}                                      // so d.f can be resolved statically. same for E.f
      """.stripMargin
    val List(c, d, e, eModule, t) = compile(code)
    assertNoInvoke(getMethod(t, "t1"))
  }

  @Test
  def inlineFromNestedClasses(): Unit = {
    val code =
      """class C {
        |  trait T { @inline final def f = 1 }
        |  class D extends T {
        |    def m(t: T) = t.f
        |  }
        |  def m(d: D) = d.f
        |}
      """.stripMargin
    val List(c, d, t) = compile(code)
    assertNoInvoke(getMethod(d, "m"))
    assertNoInvoke(getMethod(c, "m"))
  }

  @Test
  def inlineTraitCastReceiverToSelf(): Unit = {
    val code =
      """class C { def foo(x: Int) = x }
        |trait T { self: C =>
        |  @inline final def f(x: Int) = foo(x)
        |  def t1 = f(1)
        |  def t2(t: T) = t.f(2)
        |}
      """.stripMargin
    val List(c, t) = compile(code)
    val t1 = getMethod(t, "t1")
    val t2 = getMethod(t, "t2")
    val cast = TypeOp(CHECKCAST, "C")
    Set(t1, t2).foreach(m => assert(m.instructions.contains(cast), m.instructions))
  }

  @Test
  def abstractMethodWarning(): Unit = {
    val code =
      """abstract class C {
        |  @inline def foo: Int
        |}
        |class T {
        |  def t1(c: C) = c.foo
        |}
      """.stripMargin
    val warn = "C::foo()I is annotated @inline but cannot be inlined: the method is not final and may be overridden"
    var c = 0
    compile(code, allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)
  }

  @Test
  def abstractFinalMethodError(): Unit = {
    val code =
      """abstract class C {
        |  @inline final def foo: Int
        |}
        |trait T {
        |  @inline final def bar: Int
        |}
      """.stripMargin
    val err = "abstract member may not have final modifier"
    var i = 0
    compile(code, allowMessage = info => {i += 1; info.msg contains err})
    assert(i == 2, i)
  }

  @Test
  def noInlineTraitFieldAccessors(): Unit = {
    val code =
      """sealed trait T {
        |  lazy val a = 0
        |  val b = 1
        |  final lazy val c: Int = 2 // make sure it doesn't get a constant type
        |  final val d = 3
        |  final val d1: Int = 3
        |
        |  @noinline def f = 5
        |  @noinline final def g = 6
        |
        |  @noinline def h: Int
        |  @inline def i: Int
        |}
        |
        |trait U { // not sealed
        |  lazy val a = 0
        |  val b = 1
        |  final lazy val c: Int = 2 // make sure it doesn't get a constant type
        |  final val d = 3
        |  final val d1: Int = 3
        |
        |  @noinline def f = 5
        |  @noinline final def g = 6
        |
        |  @noinline def h: Int
        |  @inline def i: Int
        |}
        |
        |class C {
        |  def m1(t: T) = t.a + t.b + t.c + t.d1
        |  def m2(t: T) = t.d // inlined by the type-checker's constant folding
        |  def m3(t: T) = t.f + t.g + t.h + t.i
        |
        |  def m4(u: U) = u.a + u.b + u.c + u.d1
        |  def m5(u: U) = u.d
        |  def m6(u: U) = u.f + u.g + u.h + u.i
        |}
      """.stripMargin

    val List(c, t, u) = compile(code, allowMessage = _.msg contains "i()I is annotated @inline but cannot be inlined")
    val m1 = getMethod(c, "m1")
    assertInvoke(m1, "T", "a")
    assertInvoke(m1, "T", "b")
//    assertInvoke(m1, "T", "c") -- this lazy val is implemented purely in the trait, as it's constant, so it *can* be inlined

    assertNoInvoke(getMethod(c, "m2"))

    val m3 = getMethod(c, "m3")
    assertInvoke(m3, "T", "f")
    assertInvoke(m3, "T", "g")
    assertInvoke(m3, "T", "h")
    assertInvoke(m3, "T", "i")

    val m4 = getMethod(c, "m4")
    assertInvoke(m4, "U", "a")
    assertInvoke(m4, "U", "b")
//    assertInvoke(m4, "U", "c") -- this lazy val is implemented purely in the trait, as it's constant, so it *can* be inlined

    assertNoInvoke(getMethod(c, "m5"))

    val m6 = getMethod(c, "m6")
    assertInvoke(m6, "U", "f")
    assertInvoke(m6, "U", "g")
    assertInvoke(m6, "U", "h")
    assertInvoke(m6, "U", "i")
  }

  @Test
  def mixedNoCrashSI9111(): Unit = {
    val javaCode =
      """public final class A {
        |  public static final class T { }
        |  public static final class Inner {
        |    public static final class T { }
        |    public T newT() { return null; }
        |  }
        |}
      """.stripMargin

    val scalaCode =
      """class C {
        |  val i = new A.Inner()
        |}
      """.stripMargin

    // We don't get to see the warning about SI-9111, because it is associated with the MethodInlineInfo
    // of method newT, which is not actually used.
    // The problem is: if we reference `newT` in the scalaCode, the scala code does not compile,
    // because then SI-9111 triggers during type-checking class C, in the compiler frontend, and
    // we don't even get to the backend.
    // Nevertheless, the workaround for SI-9111 in BcodeAsmCommon.buildInlineInfoFromClassSymbol
    // is still necessary, otherwise this test crashes.
    // The warning below is the typical warning we get in mixed compilation.
    val warn =
      """failed to determine if <init> should be inlined:
        |The method <init>()V could not be found in the class A$Inner or any of its parents.
        |Note that class A$Inner could not be found on the classpath.""".stripMargin

    var c = 0

    newCompiler(extraArgs = compilerArgs + " -opt-warnings:_").compileClasses(
      scalaCode,
      List((javaCode, "A.java")),
      allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)
  }

  @Test
  def inlineInvokeSpecial(): Unit = {
    val code =
      """class A {
        |  def f1 = 0
        |}
        |class B extends A {
        |  @inline final override def f1 = 1 + super.f1 // invokespecial A.f1
        |
        |  private def f2m = 0                          // public B$$f2m in bytecode
        |  @inline final def f2 = f2m                   // invokevirtual B.B$$f2m
        |
        |  private def this(x: Int) = this()            // public in bytecode
        |  @inline final def f3 = new B()               // invokespecial B.<init>()
        |  @inline final def f4 = new B(1)              // invokespecial B.<init>(I)
        |
        |  def t1 = f1                                  // inlined
        |  def t2 = f2                                  // inlined
        |  def t3 = f3                                  // inlined
        |  def t4 = f4                                  // inlined
        |}
        |class T {
        |  def t1(b: B) = b.f1                          // cannot inline: contains a super call
        |  def t2(b: B) = b.f2                          // inlined
        |  def t3(b: B) = b.f3                          // inlined
        |  def t4(b: B) = b.f4                          // inlined
        |}
      """.stripMargin

    val warn =
      """B::f1()I is annotated @inline but could not be inlined:
        |The callee B::f1()I contains the instruction INVOKESPECIAL A.f1 ()I
        |that would cause an IllegalAccessError when inlined into class T.""".stripMargin
    var c = 0
    val List(a, b, t) = compile(code, allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)

    assertInvoke(getMethod(b, "t1"), "A", "f1")
    assertInvoke(getMethod(b, "t2"), "B", "B$$f2m")
    assertInvoke(getMethod(b, "t3"), "B", "<init>")
    assertInvoke(getMethod(b, "t4"), "B", "<init>")

    assertInvoke(getMethod(t, "t1"), "B", "f1")
    assertInvoke(getMethod(t, "t2"), "B", "B$$f2m")
    assertInvoke(getMethod(t, "t3"), "B", "<init>")
    assertInvoke(getMethod(t, "t4"), "B", "<init>")
  }

  @Test
  def dontInlineNative(): Unit = {
    val code =
      """class C {
        |  def t = System.arraycopy(null, 0, null, 0, 0)
        |}
      """.stripMargin
    val c = newCompiler(extraArgs = compilerArgs + " -Yopt-inline-heuristics:everything").compileClass(code)
    assertInvoke(getMethod(c, "t"), "java/lang/System", "arraycopy")
  }

  @Test
  def inlineMayRenderCodeDead(): Unit = {
    val code =
      """class C {
        |  @inline final def f: String = throw new Error("")
        |  @inline final def g: String = "a" + f + "b"       // after inlining f, need to run DCE, because the rest of g becomes dead.
        |  def t = g                                         // the inliner requires no dead code when inlining g (uses an Analyzer).
        |}
      """.stripMargin

    val List(c) = compile(code)
    assertInvoke(getMethod(c, "t"), "java/lang/Error", "<init>")
  }

  @Test
  def noRedunantNullChecks(): Unit = {
    val code =
      """class C {
        |  @inline final def f: String = "hai!"
        |  def t(c: C) = {c.f; c.f} // null check on the first, but not the second
        |}
      """.stripMargin

    val List(c) = compile(code)
    val t = getInstructions(c, "t")
    assertNoInvoke(t)
    assert(1 == t.collect({case Ldc(_, "hai!") => }).size)     // push-pop eliminates the first LDC("hai!")
    assert(1 == t.collect({case Jump(IFNONNULL, _) => }).size) // one single null check
  }

  @Test
  def inlineIndyLambda(): Unit = {
    val code =
      """object M {
        |  @inline def m(s: String) = {
        |    val f = (x: String) => x.trim
        |    f(s)
        |  }
        |}
        |class C {
        |  @inline final def m(s: String) = {
        |    val f = (x: String) => x.trim
        |    f(s)
        |  }
        |  def t1 = m("foo")
        |  def t2 = M.m("bar")
        |}
      """.stripMargin

    val List(c, _, _) = compile(code)

    val t1 = getMethod(c, "t1")
    assertNoIndy(t1)
    // the indy call is inlined into t, and the closure elimination rewrites the closure invocation to the body method
    assertInvoke(t1, "C", "$anonfun$m$2")

    val t2 = getMethod(c, "t2")
    assertNoIndy(t2)
    assertInvoke(t2, "M$", "$anonfun$m$1")
  }

  @Test
  def inlinePostRequests(): Unit = {
    val code =
      """class C {
        |  final def f = 10
        |  final def g = f + 19
        |  final def h = g + 29
        |  final def i = h + 39
        |}
      """.stripMargin

    val List(c) = compile(code)
    val hMeth = getAsmMethod(c, "h")
    val gMeth = getAsmMethod(c, "g")
    val iMeth = getAsmMethod(c, "i")
    val fCall = getCallsite(gMeth, "f")
    val gCall = getCallsite(hMeth, "g")
    val hCall = getCallsite(iMeth, "h")

    val warning = inliner.canInlineBody(gCall)
    assert(warning.isEmpty, warning)

    inliner.inline(InlineRequest(hCall,
      post = List(InlineRequest(gCall,
        post = List(InlineRequest(fCall, Nil, null)), null)), null))
    assertNoInvoke(convertMethod(iMeth)) // no invoke in i: first h is inlined, then the inlined call to g is also inlined, etc for f
    assertInvoke(convertMethod(gMeth), "C", "f") // g itself still has the call to f
  }

  @Test
  def postRequestSkipAlreadyInlined(): Unit = {
    val code =
      """class C {
        |  final def a = 10
        |  final def b = a + 20
        |  final def c = b + 30
        |  final def d = c + 40
        |}
      """.stripMargin

    val List(cl) = compile(code)
    val List(b, c, d) = List("b", "c", "d").map(getAsmMethod(cl, _))
    val aCall = getCallsite(b, "a")
    val bCall = getCallsite(c, "b")
    val cCall = getCallsite(d, "c")

    inliner.inline(InlineRequest(bCall, Nil, null))

    val req = InlineRequest(cCall,
      List(InlineRequest(bCall,
        List(InlineRequest(aCall, Nil, null)), null)), null)
    inliner.inline(req)

    assertNoInvoke(convertMethod(d))
  }

  @Test
  def inlineAnnotatedCallsite(): Unit = {
    val code =
      """class C {
        |  final def a(x: Int, f: Int => Int): Int = f(x)
        |  final def b(x: Int) = x
        |  final def c = 1
        |  final def d[T] = 2
        |  final def e[T](x: T) = c
        |  final def f[T](x: T) = println(x)
        |  final def g(x: Int)(y: Int) = x
        |
        |  def t1 = a(10, _ + 1)
        |  def t2 = a(10, _ + 1): @noinline
        |  def t3 = b(3)
        |  def t4 = b(3): @inline
        |  def t5 = c: @inline
        |  def t6 = d[Int]: @inline
        |  def t7 = e[Int](2): @inline
        |  def t8 = f[Int](2): @inline
        |  def t9 = g(1)(2): @inline
        |}
      """.stripMargin

    val List(c) = compile(code)
    assertInvoke(getMethod(c, "t1"), "C", "$anonfun$t1$1")
    assertInvoke(getMethod(c, "t2"), "C", "a")
    assertInvoke(getMethod(c, "t3"), "C", "b")
    assertNoInvoke(getMethod(c, "t4"))
    assertNoInvoke(getMethod(c, "t5"))
    assertNoInvoke(getMethod(c, "t6"))
    assertInvoke(getMethod(c, "t7"), "C", "c")
    assertInvoke(getMethod(c, "t8"), "scala/Predef$", "println")
    assertNoInvoke(getMethod(c, "t9"))
  }

  @Test
  def inlineNoInlineOverride(): Unit = {
    val code =
      """class C {
        |  @inline   final def f1(x: Int) = x
        |  @noinline final def f2(x: Int) = x
        |            final def f3(x: Int) = x
        |
        |  def t1 = f1(1)             // inlined
        |  def t2 = f2(1)             // not inlined
        |  def t3 = f1(1): @noinline  // not inlined
        |  def t4 = f2(1): @inline    // not inlined (cannot override the def-site @noinline)
        |  def t5 = f3(1): @inline    // inlined
        |  def t6 = f3(1): @noinline  // not inlined
        |
        |  def t7 = f1(1) + (f3(1): @inline)   // without parenthesis, the ascription encloses the entire expression..
        |  def t8 = f1(1) + (f1(1): @noinline)
        |  def t9 = f1(1) + f1(1) : @noinline  // the ascription goes on the entire expression, so on the + invocation.. both f1 are inlined
        |}
      """.stripMargin

    val List(c) = compile(code)
    assertNoInvoke(getMethod(c, "t1"))
    assertInvoke(getMethod(c, "t2"), "C", "f2")
    assertInvoke(getMethod(c, "t3"), "C", "f1")
    assertInvoke(getMethod(c, "t4"), "C", "f2")
    assertNoInvoke(getMethod(c, "t5"))
    assertInvoke(getMethod(c, "t6"), "C", "f3")
    assertNoInvoke(getMethod(c, "t7"))
    assertInvoke(getMethod(c, "t8"), "C", "f1")
    assertNoInvoke(getMethod(c, "t9"))
  }

  @Test
  def inlineHigherOrder(): Unit = {
    val code =
      """class C {
        |  final def h(f: Int => Int): Int = f(0)
        |  def t1 = h(x => x + 1)
        |  def t2 = {
        |    val fun = (x: Int) => x + 1
        |    h(fun)
        |  }
        |  def t3(f: Int => Int) = h(f)
        |  def t4(f: Int => Int) = {
        |    val fun = f
        |    h(fun)
        |  }
        |  def t5 = h(Map(0 -> 10)) // not currently inlined
        |}
      """.stripMargin

    val List(c) = compile(code)
    assertInvoke(getMethod(c, "t1"), "C", "$anonfun$t1$1")
    assertInvoke(getMethod(c, "t2"), "C", "$anonfun$t2$1")
    assertInvoke(getMethod(c, "t3"), "scala/Function1", "apply$mcII$sp")
    assertInvoke(getMethod(c, "t4"), "scala/Function1", "apply$mcII$sp")
    assertInvoke(getMethod(c, "t5"), "C", "h")
  }

  @Test
  def twoStepNoInlineHandler(): Unit = {
    val code =
      """class C {
        |  @inline final def f = try 1 catch { case _: Throwable => 2 }
        |  @inline final def g = f
        |  def t = println(g)       // cannot inline g onto non-empty stack once that f was inlined into g
        |}
      """.stripMargin

    val warn =
      """C::g()I is annotated @inline but could not be inlined:
        |The operand stack at the callsite in C::t()V contains more values than the
        |arguments expected by the callee C::g()I. These values would be discarded
        |when entering an exception handler declared in the inlined method.""".stripMargin

    val List(c) = compile(code, allowMessage = _.msg contains warn)
    assertInvoke(getMethod(c, "t"), "C", "g")
  }

  @Test
  def twoStepNoInlinePrivate(): Unit = {
    val code =
      """class C {
        |  @inline final def g = {
        |    @noinline def f = 0
        |    f
        |  }
        |  @inline final def h = g   // after inlining g, h has an invocate of private method f$1
        |}
        |class D {
        |  def t(c: C) = c.h  // cannot inline
        |}
      """.stripMargin

    val warn =
      """C::h()I is annotated @inline but could not be inlined:
        |The callee C::h()I contains the instruction INVOKESTATIC C.f$1 ()I
        |that would cause an IllegalAccessError when inlined into class D.""".stripMargin

    val List(c, d) = compile(code, allowMessage = _.msg contains warn)
    assertInvoke(getMethod(c, "h"), "C", "f$1")
    assertInvoke(getMethod(d, "t"), "C", "h")
  }

  @Test
  def twoStepInlinePrivate(): Unit = {
    val code =
      """class C {
        |  @inline final def g = {  // initially, g invokes the private method f$1, but then f$1 is inlined
        |    @inline def f = 0
        |    f
        |  }
        |}
        |class D {
        |  def t(c: C) = c.g  // can inline
        |}
      """.stripMargin

    val List(c, d) = compile(code)
    assertNoInvoke(getMethod(c, "g"))
    assertNoInvoke(getMethod(d, "t"))
  }

  @Test
  def optimizeSpecializedClosures(): Unit = {
    val code =
      """class ValKl(val x: Int) extends AnyVal
        |
        |class C {
        |  def t1 = {
        |    // IndyLambda: SAM type is JFunction1$mcII$sp, SAM is apply$mcII$sp(I)I, body method is $anonfun(I)I
        |    val f = (x: Int) => x + 1
        |    // invocation of apply$mcII$sp(I)I, matches the SAM in IndyLambda. no boxing / unboxing needed.
        |    f(10)
        |    // opt: re-write the invocation to the body method
        |  }
        |
        |  @inline final def m1a(f: Long => Int) = f(1l)
        |  def t1a = m1a(l => l.toInt) // after inlining m1a, we have the same situation as in t1
        |
        |  def t2 = {
        |    // there is no specialized variant of Function2 for this combination of types, so the IndyLambda has to create a generic Function2.
        |    // IndyLambda: SAM type is JFunction2, SAM is apply(ObjectObject)Object, body method is $anonfun$adapted(ObjectObject)Object
        |    val f = (b: Byte, i: Int) => i + b
        |    // invocation of apply(ObjectOjbect)Object, matches SAM in IndyLambda. arguments are boxed, result unboxed.
        |    f(1, 2)
        |    // opt: re-wrtie to $anonfun$adapted
        |    // inline that call, then we get box-unbox pairs (can be eliminated) and a call to $anonfun(BI)I
        |  }
        |
        |  def t3 = {
        |    // similar to t2: for functions with value class parameters, IndyLambda always uses the generic Function version.
        |    // IndyLambda: SAM type is JFunction1, SAM is apply(Object)Object, body method is $anonfun$adapted(Object)Object
        |    val f = (a: ValKl) => a
        |    // invocation of apply(Object)Object, ValKl instance is created, result extracted
        |    f(new ValKl(1))
        |    // opt: re-write to $anonfun$adapted.
        |    // inline that call, then we get value class instantiation-extraction pairs and a call to $anonfun(I)I
        |  }
        |
        |  def t4 = {
        |    // IndyLambda: SAM type is JFunction1$mcII$sp, SAM is apply$mcII$sp(I)I, body method is $anonfun(I)I
        |    val f: Int => Any = (x: Int) => 1
        |    // invocation of apply(Object)Object, argument is boxed. method name and type doesn't match IndyLambda.
        |    f(10)
        |    // opt: rewriting to the body method requires inserting an unbox operation for the argument, and a box operation for the result
        |    // that produces a box-unbox pair and a call to $anonfun(I)I
        |  }
        |
        |
        |  @inline final def m4a[T, U, V](f: (T, U) => V, x: T, y: U) = f(x, y) // invocation to generic apply(ObjectObject)Object
        |  def t4a = m4a((x: Int, y: Double) => 1l + x + y.toLong, 1, 2d) // IndyLambda uses specilized JFunction2$mcJID$sp. after inlining m4a, similar to t4.
        |
        |  def t5 = {
        |    // no specialization for the comibnation of primitives
        |    // IndyLambda: SAM type is JFunction2, SAM is generic apply, body method is $anonfun$adapted
        |    val f: (Int, Byte) => Any = (x: Int, b: Byte) => 1
        |    // invocation of generic apply.
        |    f(10, 3)
        |    // opt: re-write to $anonfun$adapted, inline that method. generates box-unbox pairs and a call to $anonfun(IB)I
        |  }
        |
        |  def t5a = m4a((x: Int, y: Byte) => 1, 12, 31.toByte) // similar to t5 after inlining m4a
        |
        |  // m6$mIVc$sp invokes apply$mcVI$sp
        |  @inline final def m6[@specialized(Int) T, @specialized(Unit) U](f: T => U, x: T): Unit = f(x)
        |  // IndyLambda: JFunction1$mcVI$sp, SAM is apply$mcVI$sp, body method $anonfun(I)V
        |  // invokes m6$mIVc$sp (Lscala/Function1;I)V
        |  def t6 = m6((x: Int) => (), 10)
        |  // opt: after inlining m6, the closure method invocation (apply$mcVI$sp) matches the IndyLambda, the call can be rewritten, no boxing
        |
        |  // m7 invokes apply
        |  @inline final def m7[@specialized(Boolean) T, @specialized(Int) U](f: T => U, x: T): Unit = f(x)
        |  // IndyLambda: JFunction1, SAM is apply(Object)Object, body method is $anonfun$adapted(Obj)Obj
        |  // `true` is boxed before passing to m7
        |  def t7 = m7((x: Boolean) => (), true)
        |  // opt: after inlining m7, the apply call is re-written to $anonfun$adapted, which is then inlined.
        |  // we get a box-unbox pair and a call to $anonfun(Z)V
        |
        |
        |  // invokes the generic apply(ObjObj)Obj
        |  @inline final def m8[T, U, V](f: (T, U) => V, x: T, y: U) = f(x, y)
        |  // IndyLambda: JFunction2$mcJID$sp, SAM is apply$mcJID$sp, body method $anonfun(ID)J
        |  // boxes the int and double arguments and calls m8, unboxToLong the result
        |  def t8 = m8((x: Int, y: Double) => 1l + x + y.toLong, 1, 2d)
        |  // opt: after inlining m8, rewrite to the body method $anonfun(ID)J, which requires inserting unbox operations for the params, box for the result
        |  // the box-unbox pairs can then be optimized away
        |
        |  // m9$mVc$sp invokes apply$mcVI$sp
        |  @inline final def m9[@specialized(Unit) U](f: Int => U): Unit = f(1)
        |  // IndyLambda: JFunction1, SAM is apply(Obj)Obj, body method $anonfun$adapted(Ojb)Obj
        |  // invocation of m9$mVc$sp
        |  def t9 = m9(println)
        |  // opt: after inlining m9, rewrite to $anonfun$adapted(Ojb)Obj, which requires inserting a box operation for the parameter.
        |  // then we inline $adapted, which has signature (Obj)V. the `BoxedUnit.UNIT` from the body of $anonfun$adapted is eliminated by push-pop
        |
        |  def t9a = (1 to 10) foreach println // similar to t9
        |
        |  def intCons(i: Int): Unit = ()
        |  // IndyLambda: JFunction1$mcVI$sp, SAM is apply$mcVI$sp, body method $anonfun(I)V
        |  def t10 = m9(intCons)
        |  // after inlining m9, rewrite the apply$mcVI$sp call to the body method, no adaptations required
        |
        |  def t10a = (1 to 10) foreach intCons // similar to t10
        |}
      """.stripMargin
    val List(c, _, _) = compile(code)

    assertSameSummary(getMethod(c, "t1"), List(BIPUSH, "$anonfun$t1$1", IRETURN))
    assertSameSummary(getMethod(c, "t1a"), List(LCONST_1, "$anonfun$t1a$1", IRETURN))
    assertSameSummary(getMethod(c, "t2"), List(ICONST_1, ICONST_2, "$anonfun$t2$1",IRETURN))

    // val a = new ValKl(n); new ValKl(anonfun(a.x)).x
    // value class instantiation-extraction should be optimized by boxing elim
    assertSameSummary(getMethod(c, "t3"), List(
      NEW, DUP, ICONST_1, "<init>", ASTORE,
      NEW, DUP, ALOAD, "x",
      "$anonfun$t3$1",
      "<init>",
      "x", IRETURN))

    assertSameSummary(getMethod(c, "t4"), List(BIPUSH, "$anonfun$t4$1", "boxToInteger", ARETURN))
    assertSameSummary(getMethod(c, "t4a"), List(ICONST_1, LDC, "$anonfun$t4a$1", LRETURN))
    assertSameSummary(getMethod(c, "t5"), List(BIPUSH, ICONST_3, "$anonfun$t5$1", "boxToInteger", ARETURN))
    assertSameSummary(getMethod(c, "t5a"), List(BIPUSH, BIPUSH, I2B, "$anonfun$t5a$1", IRETURN))
    assertSameSummary(getMethod(c, "t6"), List(BIPUSH, "$anonfun$t6$1", RETURN))
    assertSameSummary(getMethod(c, "t7"), List(ICONST_1, "$anonfun$t7$1", RETURN))
    assertSameSummary(getMethod(c, "t8"), List(ICONST_1, LDC, "$anonfun$t8$1", LRETURN))
    assertSameSummary(getMethod(c, "t9"), List(ICONST_1, "boxToInteger", "$anonfun$t9$1", RETURN))

    // t9a inlines Range.foreach, which is quite a bit of code, so just testing the core
    assertInvoke(getMethod(c, "t9a"), "C", "$anonfun$t9a$1")
    assertInvoke(getMethod(c, "t9a"), "scala/runtime/BoxesRunTime", "boxToInteger")

    assertSameSummary(getMethod(c, "t10"), List(
      ICONST_1, ISTORE,
      ALOAD, ILOAD,
      "$anonfun$t10$1", RETURN))

    // t10a inlines Range.foreach
    assertInvoke(getMethod(c, "t10a"), "C", "$anonfun$t10a$1")
    assertDoesNotInvoke(getMethod(c, "t10a"), "boxToInteger")
  }

  @Test
  def refElimination(): Unit = {
    val code =
      """class C {
        |  def t1 = {
        |    var i = 0
        |    @inline def inner() = i += 1
        |    inner()
        |    i
        |  }
        |
        |  final def m(f: Int => Unit) = f(10)
        |  def t2 = {
        |    var x = -1                  // IntRef not yet eliminated: closure elimination does not
        |    m(i => if (i == 10) x = 1)  // yet inline the anonfun method, need to improve the heuristsics
        |    x
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)
    assertSameCode(getMethod(c, "t1"), List(Op(ICONST_0), Op(ICONST_1), Op(IADD), Op(IRETURN)))
    assertEquals(getMethod(c, "t2").instructions collect { case i: Invoke => i.owner +"."+ i.name }, List(
      "scala/runtime/IntRef.create", "C.$anonfun$t2$1"))
  }

  @Test
  def tupleElimination(): Unit = {
    val code =
      """class C {
        |  @inline final def tpl[A, B](a: A, b: B) = (a, b)
        |  @inline final def t_1[A, B](t: (A, B)) = t._1
        |  @inline final def t_2[A, B](t: (A, B)) = t._2
        |
        |  def t1 = {
        |    val t = (3, 4)  // specialized tuple
        |    t_1(t) + t_2(t) // invocations to generic _1 / _2, box operation inserted when eliminated
        |  }
        |
        |  def t2 = {
        |    val t = tpl(1, 2) // generic Tuple2[Integer, Integer] created
        |    t._1 + t._2       // invokes the specialized _1$mcI$sp, eliminating requires adding an unbox operation
        |  }
        |
        |  @inline final def m = (1, 3)
        |  def t3 = {
        |    val (a, b) = m
        |    a - b
        |  }
        |
        |  def t4 = {
        |    val ((a, b), (c, d)) = (m, m)
        |    a + b + c + d
        |  }
        |
        |  def t5 = m match {
        |    case (1, y) => y
        |    case (x, y) => x * y
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)
    assertSameCode(getMethod(c, "t1"), List(Op(ICONST_3), Op(ICONST_4), Op(IADD), Op(IRETURN)))
    assertSameCode(getMethod(c, "t2"), List(Op(ICONST_1), Op(ICONST_2), Op(IADD), Op(IRETURN)))
    assertSameCode(getMethod(c, "t3"), List(Op(ICONST_1), Op(ICONST_3), Op(ISUB), Op(IRETURN)))
    assertNoInvoke(getMethod(c, "t4"))
    assertNoInvoke(getMethod(c, "t5"))
  }

  @Test
  def redundantCasts(): Unit = {

    // we go through the hoop of inlining the casts because erasure eliminates `asInstanceOf` calls
    // that are statically known to succeed. For example the following cast is removed by erasure:
    //   `(if (b) c else d).asInstanceOf[C]`

    val code =
      """class C {
        |  @inline final def asO(a: Any) = a.asInstanceOf[Object]
        |  @inline final def asC(a: Any) = a.asInstanceOf[C]
        |  @inline final def asD(a: Any) = a.asInstanceOf[D]
        |
        |  def t1(c: C) = asC(c) // eliminated
        |  def t2(c: C) = asO(c) // eliminated
        |  def t3(c: Object) = asC(c) // not elimianted
        |  def t4(c: C, d: D, b: Boolean) = asC(if (b) c else d) // not eliminated: lub of two non-equal reference types approximated with Object
        |  def t5(c: C, d: D, b: Boolean) = asO(if (b) c else d)
        |  def t6(c: C, cs: Array[C], b: Boolean) = asO(if (b) c else cs)
        |}
        |class D extends C
      """.stripMargin
    val List(c, _) = compile(code)
    def casts(m: String) = getInstructions(c, m) collect { case TypeOp(CHECKCAST, tp) => tp }
    assertSameCode(getMethod(c, "t1"), List(VarOp(ALOAD, 1), Op(ARETURN)))
    assertSameCode(getMethod(c, "t2"), List(VarOp(ALOAD, 1), Op(ARETURN)))
    assertSameCode(getMethod(c, "t3"), List(VarOp(ALOAD, 1), TypeOp(CHECKCAST, "C"), Op(ARETURN)))
    assertEquals(casts("t4"), List("C"))
    assertEquals(casts("t5"), Nil)
    assertEquals(casts("t6"), Nil)
  }

  @Test
  def inlineFromSealed(): Unit = {
    val code =
      """sealed abstract class Foo {
        |  @inline def bar(x: Int) = x + 1
        |}
        |object Foo {
        |  def mkFoo(): Foo = new Baz2
        |}
        |
        |object Baz1 extends Foo
        |final class Baz2 extends Foo
        |
        |object Test {
        |  def f = Foo.mkFoo() bar 10
        |}
      """.stripMargin

    val cls = compile(code)
    val test = findClass(cls, "Test$")
    assertSameSummary(getMethod(test, "f"), List(
      GETSTATIC, "mkFoo",
      BIPUSH, ISTORE,
      IFNONNULL, ACONST_NULL, ATHROW, -1 /*label*/,
      ILOAD, ICONST_1, IADD, IRETURN))
  }

  @Test // a test taken from the test suite for the 2.11 inliner
  def oldInlineHigherOrderTest(): Unit = {
    val code =
      """class C {
        |  private var debug = false
        |  @inline private def ifelse[T](cond: => Boolean, ifPart: => T, elsePart: => T): T = if (cond) ifPart else elsePart
        |  final def t = ifelse(debug, 1, 2)
        |}
      """.stripMargin
    val List(c) = compile(code)

    // box-unbox will clean it up
    assertSameSummary(getMethod(c, "t"), List(
      ALOAD, "$anonfun$t$1", IFEQ /*A*/,
      "$anonfun$t$2", IRETURN,
      -1 /*A*/, "$anonfun$t$3", IRETURN))
  }

  @Test
  def inlineProject(): Unit = {
    val codeA = "final class A { @inline def f = 1 }"
    val codeB = "class B { def t(a: A) = a.f }"
    // tests that no warning is emitted
    val List(a, b) = compileClassesSeparately(List(codeA, codeB), extraArgs = "-opt:l:project -opt-warnings")
    assertInvoke(getMethod(b, "t"), "A", "f")
  }

  @Test
  def sd86(): Unit = {
    val code =
      """trait T1 { @inline def f = 999 }
        |trait T2 { self: T1 => @inline override def f = 1 } // note that f is not final
        |class C extends T1 with T2
      """.stripMargin
    val List(c, t1, t2) = compile(code, allowMessage = _ => true)
    // the forwarder C.f is inlined, so there's no invocation
    assertSameSummary(getMethod(c, "f"), List(ICONST_1, IRETURN))
  }

  @Test
  def sd140(): Unit = {
    val code =
      """trait T { @inline def f = 0 }
        |trait U extends T { @inline override def f = 1 }
        |trait V extends T { def m = 0 }
        |final class K extends V with U { override def m = super[V].m }
        |class C { def t = (new K).f }
      """.stripMargin
    val c :: _ = compile(code)
    assertSameSummary(getMethod(c, "t"), List(NEW, "<init>", ICONST_1, IRETURN))  // ICONST_1, U.f is inlined (not T.f)
  }

  @Test
  def inlineArrayForeach(): Unit = {
    val code =
      """class C {
        |  def consume(x: Int) = ()
        |  def t(a: Array[Int]): Unit = a foreach consume
        |}
      """.stripMargin
    val List(c) = compile(code)
    val t = getMethod(c, "t")
    assertNoIndy(t)
    assertInvoke(t, "C", "$anonfun$t$1")
  }

  @Test
  def t9121(): Unit = {
    val codes = List(
      """package p1
        |object Implicits {
        |  class ScalaObservable(val underlying: Any) extends AnyVal {
        |    @inline def scMap[R](f: String): Any = f.toRx
        |  }
        |  implicit class RichFunction1[T1, R](val f: String) extends AnyVal {
        |    def toRx: Any = ""
        |  }
        |}
      """.stripMargin,
      """
        |import p1.Implicits._
        |class C {
        |  def t(): Unit = new ScalaObservable("").scMap("")
        |}
      """.stripMargin)
    val c :: _ = compileClassesSeparately(codes, extraArgs = compilerArgs)
    assertInvoke(getMethod(c, "t"), "p1/Implicits$RichFunction1$", "toRx$extension")
  }

  @Test
  def keepLineNumbersPerCompilationUnit(): Unit = {
    val code1 =
      """class A {
        |  def fx(): Unit = ()
        |  @inline final def ma = {
        |    fx()
        |    1
        |  }
        |}
      """.stripMargin
    val code2 =
      """class B extends A {
        |  @inline final def mb = {
        |    fx()
        |    1
        |  }
        |}
        |class C extends B {
        |  @inline final def mc = {
        |    fx()
        |    1
        |  }
        |  def t1 = ma // no lines, not the same source file
        |  def t2 = mb // lines
        |  def t3 = mc // lines
        |}
      """.stripMargin
    notPerRun.foreach(_.clear())
    val run = compiler.newRun
    run.compileSources(List(makeSourceFile(code1, "A.scala"), makeSourceFile(code2, "B.scala")))
    val List(_, _, c) = readAsmClasses(getGeneratedClassfiles(global.settings.outputDirs.getSingleOutput.get))
    def is(name: String) = getMethod(c, name).instructions.filterNot(_.isInstanceOf[FrameEntry])

    assertSameCode(is("t1"), List(
      Label(0), LineNumber(12, Label(0)),
      VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "A", "fx", "()V", false),
      Op(ICONST_1), Op(IRETURN), Label(6)))

    assertSameCode(is("t2"), List(
      Label(0), LineNumber(3, Label(0)), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "B", "fx", "()V", false),
      Label(4), LineNumber(4, Label(4)), Op(ICONST_1), Op(IRETURN), Label(8)))

    assertSameCode(is("t3"), List(
      Label(0), LineNumber(9, Label(0)), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "fx", "()V", false),
      Label(4), LineNumber(10, Label(4)), Op(ICONST_1), Op(IRETURN), Label(8)))
  }

  @Test
  def traitHO(): Unit = {
    val code =
      """trait T {
        |  def foreach(f: Int => Unit): Unit = f(1)
        |}
        |final class C extends T {
        |  def cons(x: Int): Unit = ()
        |  def t1 = foreach(cons)
        |}
      """.stripMargin
    val List(c, t) = compile(code)
    assertNoIndy(getMethod(c, "t1"))
  }

  @Test
  def limitInlinedLocalVariableNames(): Unit = {
    val code =
      """class C {
        |  def f(x: Int): Int = x
        |  @inline final def methodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(param: Int) =
        |    f(param)
        |  @inline final def anotherMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(param: Int) =
        |    methodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(f(param))
        |  @inline final def oneMoreMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(param: Int) =
        |    anotherMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(f(param))
        |  @inline final def yetAnotherMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(param: Int) =
        |    oneMoreMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(f(param))
        |  @inline final def oneLastMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(param: Int) =
        |    yetAnotherMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(f(param))
        |  def t(p: Int) =
        |    oneLastMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(f(p)) +
        |    oneLastMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence(f(p))
        |}
      """.stripMargin

    val List(c) = compile(code)
    assertEquals(getAsmMethod(c, "t").localVariables.asScala.toList.map(l => (l.name, l.index)).sortBy(_._2),List(
      ("this",0),
      ("p",1),
      ("oneLastMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence_param",2),
      ("oneLastMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchS_yetAnotherMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFren_param",3),
      ("oneLastMethodWithVeryVeryLongNameAlmostLik_yetAnotherMethodWithVeryVeryLongNameAlmost_oneMoreMethodWithVeryVeryLongNameAlmostLik_param",4),
      ("oneLastMethodWithVeryVeryLongNam_yetAnotherMethodWithVeryVeryLong_oneMoreMethodWithVeryVeryLongNam_anotherMethodWithVeryVeryLongNam_param",5),
      ("oneLastMethodWithVeryVery_yetAnotherMethodWithVeryV_oneMoreMethodWithVeryVery_anotherMethodWithVeryVery_methodWithVeryVeryLongNam_param",6),
      ("oneLastMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchSentence_param",7),
      ("oneLastMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFrenchS_yetAnotherMethodWithVeryVeryLongNameAlmostLikeAGermanWordOrAFren_param",8),
      ("oneLastMethodWithVeryVeryLongNameAlmostLik_yetAnotherMethodWithVeryVeryLongNameAlmost_oneMoreMethodWithVeryVeryLongNameAlmostLik_param",9),
      ("oneLastMethodWithVeryVeryLongNam_yetAnotherMethodWithVeryVeryLong_oneMoreMethodWithVeryVeryLongNam_anotherMethodWithVeryVeryLongNam_param",10),
      ("oneLastMethodWithVeryVery_yetAnotherMethodWithVeryV_oneMoreMethodWithVeryVery_anotherMethodWithVeryVery_methodWithVeryVeryLongNam_param",11)))
  }
}

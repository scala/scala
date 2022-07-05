package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.JavaClearable
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.testkit.ASMConverters._
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

@RunWith(classOf[JUnit4])
class InlinerTest extends BytecodeTesting {
  import compiler._
  import global.genBCode.{bTypes, postProcessor}

  override def compilerArgs = "-opt:inline:** -Wopt"

  val inlineOnly = global.settings.optChoices.inlineFrom.expandsTo.map(c => s"-${c.name}").mkString("-opt:inline:** -opt:", ",", "")
  val inlineOnlyCompiler = cached("inlineOnlyCompiler", () => newCompiler(extraArgs = inlineOnly))

  compiler.keepPerRunCachesAfterRun(List(
    JavaClearable.forMap(bTypes.classBTypeCache),
    postProcessor.byteCodeRepository.compilingClasses,
    postProcessor.byteCodeRepository.parsedClasses,
    postProcessor.callGraph.callsites))

  import global.genBCode.postProcessor.{byteCodeRepository, callGraph, inliner}

  def checkCallsite(callsite: callGraph.Callsite, callee: MethodNode) = {
    assert(callsite.callsiteMethod.instructions.contains(callsite.callsiteInstruction), instructionsFromMethod(callsite.callsiteMethod))

    val callsiteClassNode = byteCodeRepository.classNode(callsite.callsiteClass.internalName).get
    assert(callsiteClassNode.methods.contains(callsite.callsiteMethod), callsiteClassNode.methods.asScala.map(_.name).toList)

    assert(callsite.callee.get.callee == callee, callsite.callee.get.callee.name)
  }

  def getCallsite(method: MethodNode, calleeName: String) = callGraph.callsites(method).valuesIterator.find(_.callee.get.callee.name == calleeName).get

  def gMethAndFCallsite(code: String, mod: ClassNode => Unit = _ => ()) = {
    val List(c) = { compileClass(code); compiledClassesFromCache }
    mod(c)
    val gMethod = getAsmMethod(c, "g")
    val fCall = getCallsite(gMethod, "f")
    (gMethod, fCall)
  }

  def canInlineTest(code: String, mod: ClassNode => Unit = _ => ()): Option[OptimizerWarning] = {
    val cs = gMethAndFCallsite(code, mod)._2
    inliner.earlyCanInlineCheck(cs) orElse inliner.canInlineCallsite(cs)
  }

  def inlineTest(code: String, mod: ClassNode => Unit = _ => ()): MethodNode = {
    val (gMethod, fCall) = gMethAndFCallsite(code, mod)
    inliner.inlineCallsite(fCall)
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
        VarOp(ALOAD, 0),
        Op(POP), // pop receiver - we know the stack value is also in the local variable 0, so we use that local in the inlined code
        Op(ICONST_1), Jump(GOTO, Label(10)), // load return value
        Label(10),
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "f", "()I", false), Op(IADD), Op(IRETURN)))

    // line numbers are kept, so there's a line 2 (from the inlined f)
    assert(gConv.instructions exists {
      case LineNumber(2, _) => true
      case _ => false
    }, gConv.instructions.filter(_.isInstanceOf[LineNumber]))

    assert(gConv.localVars.map(_.name).sorted == List("this"), gConv.localVars)
    assert(g.maxStack == 2 && g.maxLocals == 2, s"${g.maxLocals} - ${g.maxStack}")
  }

  @Test
  def nothingTypedOK(): Unit = {
    val code =
      """class C {
        |  def f: Nothing = ??? : @noinline
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

    val gBeforeLocalOpt = VarOp(ALOAD, 0) :: Op(POP) :: invokeQQQ ::: List(
      Jump(GOTO, Label(14)),
      Label(14),
      Op(ATHROW))

    assertSameCode(convertMethod(g), gBeforeLocalOpt)

    global.genBCode.postProcessor.localOpt.methodOptimizations(g, "C")
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
        |    def g: Int = f: @noinline
        |  }
        |}
        |package b {
        |  class D {
        |    def h(c: a.C): Int = c.g + 1
        |  }
        |}
      """.stripMargin

    val List(c, d) = { compileClasses(code); compiledClassesFromCache }
    val hMeth = getAsmMethod(d, "h")
    val gCall = getCallsite(hMeth, "g")
    val r = inliner.canInlineCallsite(gCall)
    assert(r.nonEmpty && r.get.isInstanceOf[IllegalAccessInstructions], r)
  }

  @Test
  def inlineSimpleAtInline(): Unit = {
    val code =
      """class C {
        |  @inline final def f = 0
        |  final def g = 1
        |
        |  def test = f + (g: @noinline)
        |}
      """.stripMargin
    val cCls= compileClass(code)
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
    val List(c) = { compileClass(code); compiledClassesFromCache }
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
    val List(c) = { compileClass(code); compiledClassesFromCache }
    val methods @ List(f, g, h) = c.methods.asScala.filter(_.name.length == 1).sortBy(_.name).toList
    val List(fIns, gIns, hIns) = methods.map(instructionsFromMethod(_).dropNonOp)
    val invokeG = Invoke(INVOKEVIRTUAL, "C", "g", "()I", false)
    // first round
    //   - no inlining into f, these requests are elided
    //   - h = g + g
    //   - g = g + g
    // second round
    //   - h changed. both invocations of `g` are inlined (no callsite of `g` was inlined into h
    //     before, so there's no loop). therefore: h = g + g + g + g
    //   - g changed. both invocation are self-recursive, therefore not inlined
    // third round
    //   - h changed. each invocation of `g` ended up in `h` by inlining `g`. in other words, the
    //     inline chain for `g` contains a call to `g`. therefore nothing else is inlined.
    assert(fIns.count(_ == invokeG) == 2, fIns)
    assert(gIns.count(_ == invokeG) == 2, gIns)
    assert(hIns.count(_ == invokeG) == 4, hIns)

    assert(callGraph.callsites.valuesIterator.flatMap(_.valuesIterator).size == 9, callGraph.callsites)
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
        |  @inline def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = {
        |    System.arraycopy(src, srcPos, dest, destPos, length)
        |  }
        |}
        |class C {
        |  def f(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = {
        |    Platform.arraycopy(src, srcPos, dest, destPos, length)
        |  }
        |}
      """.stripMargin
    val List(c, _, _) = compileClasses(code)
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
    @annotation.unused val c = compileClass(code)
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
    val List(c, t) = compileClasses(code)
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
    val c = compileClass(code)
    // no more invoke, f is inlined
    assertNoInvoke(getMethod(c, "g"))
  }

  @Test
  def inlineStaticCall(): Unit = {
    val code =
      """class C {
        |  def f = Integer.lowestOneBit(103): @noinline
        |}
      """.stripMargin

    val List(c) = { compileClass(code); compiledClassesFromCache }
    val fMeth = getAsmMethod(c, "f")
    val call = getCallsite(fMeth, "lowestOneBit")

    val warning = inliner.canInlineCallsite(call)
    assert(warning.isEmpty, warning)

    inliner.inlineCallsite(call)
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
        |    math.max(a, math.min(10, a - 1): @noinline): @noinline
        |  }
        |
        |  @inline final def f2(x: Int): Unit = {
        |    val a = x + 1
        |    println(runtime.BoxesRunTime.boxToInteger(math.max(a, 10): @noinline): @noinline): @noinline
        |  }
        |
        |  def g1 = println(runtime.BoxesRunTime.boxToInteger(f1(32)): @noinline): @noinline
        |  def g2 = println(f2(32)): @noinline
        |}
      """.stripMargin

    // use a compiler without local optimizations (cleanups)
    val c = inlineOnlyCompiler.compileClass(code)
    val ms @ List(f1, f2, g1, g2) = c.methods.asScala.filter(_.name.length == 2).toList

    // stack height at callsite of f1 is 1, so max of g1 after inlining is max of f1 + 1
    assert(g1.maxStack == 7 && f1.maxStack == 6, s"${g1.maxStack} - ${f1.maxStack}")

    // locals in f1: this, x, a
    // locals in g1 after inlining: this, x, a (this is reused)
    assert(g1.maxLocals == 3 && f1.maxLocals == 3, s"${g1.maxLocals} - ${f1.maxLocals}")

    // like maxStack in g1 / f1
    assert(g2.maxStack == 5 && f2.maxStack == 4, s"${g2.maxStack} - ${f2.maxStack}")

    // like maxLocals for g1 / f1
    assert(g2.maxLocals == 3 && f2.maxLocals == 3, s"${g2.maxLocals} - ${f2.maxLocals}")
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
    val b = compileClass(scalaCode, List((javaCode, "A.java")), allowMessage = i => {c += 1; i.msg contains warn})
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
    val List(c, t) = compileClasses(code)
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
    val List(c, t, u) = compileClasses(code)
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
    val warn = "::f()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden."
    var count = 0
    val List(c, t) = compileClasses(code, allowMessage = i => {count += 1; i.msg contains warn})
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
    val List(c, t) = compileClasses(code)
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
    val warn = "T::f()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden."
    var count = 0
    val List(c, oMirror, oModule, t) = compileClasses(code, allowMessage = i => {count += 1; i.msg contains warn})
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

    val List(assembly, c, t) = compileClasses(code)

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
        |  def m1a = g1           // call to accessor, inlined, then call to f inlined in the next round, we get ICONST_1
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
        |  def m1b = g1           // inlined, then f is inlined in the next round, we get ICONST_1
        |  def m2b = g2b          // inlined, we get the interface call to T1.f
        |  def m3b = f            // inlined, we get ICONST_1
        |
        |  def m4b(t: T1) = t.f   // T1.f is not final, so not inlined, interface call to T1.f
        |  def m5b(t: T2b) = t.f  // inlined, ICONST_1
        |}
      """.stripMargin

    val warning = "T1::f()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden."
    var count = 0
    val List(ca, cb, t1, t2a, t2b) = compileClasses(code, allowMessage = i => {count += 1; i.msg contains warning})
    assert(count == 4, count) // see comments, f is not inlined 4 times

    assertNoInvoke(getMethod(t2a, "g2a"))
    assertInvoke(getMethod(t2b, "g2b"), "T1", "f")

    assertNoInvoke(getMethod(ca, "m1a"))
    assertNoInvoke(getMethod(ca, "m2a"))            // no invoke, see comment on def g2a
    assertNoInvoke(getMethod(ca, "m3a"))
    assertInvoke(getMethod(ca, "m4a"), "T1", "f")
    assertNoInvoke(getMethod(ca, "m5a"))

    assertNoInvoke(getMethod(cb, "m1b"))
    assertInvoke(getMethod(cb, "m2b"), "T1", "f")  // invoke, see comment on def g2b << TODO: check why not inlined in later round??? >>
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
    val List(c, d, e, eModule, t) = compileClasses(code)
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
    val List(c, d, t) = compileClasses(code)
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
    val List(c, t) = compileClasses(code)
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
    val warn = "C::foo()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden."
    var c = 0
    compileClasses(code, allowMessage = i => {c += 1; i.msg contains warn})
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
    compileClasses(code, allowMessage = info => {i += 1; info.msg contains err})
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

    val List(c, t, u) = compileClasses(code, allowMessage = _.msg contains "::i()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden.")
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

    // We don't get to see the warning about scala/bug#9111, because it is associated with the MethodInlineInfo
    // of method newT, which is not actually used.
    // The problem is: if we reference `newT` in the scalaCode, the scala code does not compile,
    // because then scala/bug#9111 triggers during type-checking class C, in the compiler frontend, and
    // we don't even get to the backend.
    // Nevertheless, the workaround for scala/bug#9111 in BcodeAsmCommon.buildInlineInfoFromClassSymbol
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
    val List(a, b, t) = compileClasses(code, allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)

    assertInvoke(getMethod(b, "t1"), "A", "f1")
    assertNoInvoke(getMethod(b, "t2"))
    assertInvoke(getMethod(b, "t3"), "B", "<init>")
    assertInvoke(getMethod(b, "t4"), "B", "<init>")

    assertInvoke(getMethod(t, "t1"), "B", "f1")
    assertNoInvoke(getMethod(t, "t2"))
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

    val c = compileClass(code)
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

    val c = compileClass(code)
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

    val List(c, _, _) = compileClasses(code)

    val t1 = getMethod(c, "t1")
    assertNoIndy(t1)
    // the indy call is inlined into t, and the closure elimination rewrites the closure invocation to the body method
    assertInvoke(t1, "java/lang/String", "trim")

    val t2 = getMethod(c, "t2")
    assertNoIndy(t2)
    assertInvoke(t2, "java/lang/String", "trim")
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

    val c= compileClass(code)
    assertNoInvoke(getMethod(c, "t1"))
    assertInvoke(getMethod(c, "t2"), "C", "a")
    assertNoInvoke(getMethod(c, "t3"))
    assertNoInvoke(getMethod(c, "t4"))
    assertNoInvoke(getMethod(c, "t5"))
    assertNoInvoke(getMethod(c, "t6"))
    assertNoInvoke(getMethod(c, "t7"))
    assertInvoke(getMethod(c, "t8"), "scala/Console$", "println")
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
        |  def t4 = f2(1): @inline    // inlined
        |  def t5 = f3(1): @inline    // inlined
        |  def t6 = f3(1): @noinline  // not inlined
        |
        |  def t7 = f1(1) + (f3(1): @inline)   // without parenthesis, the ascription encloses the entire expression..
        |  def t8 = f1(1) + (f1(1): @noinline)
        |  def t9 = f1(1) + f1(1) : @noinline  // the ascription goes on the entire expression, so on the + invocation.. both f1 are inlined
        |}
      """.stripMargin

    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "t1"))
    assertInvoke(getMethod(c, "t2"), "C", "f2")
    assertInvoke(getMethod(c, "t3"), "C", "f1")
    assertNoInvoke(getMethod(c, "t4"))
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
        |  final def h(f: Int => Int): Int = f(0) + f(0)
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

    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "t1"))
    assertNoInvoke(getMethod(c, "t2"))
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

    val c = compileClass(code, allowMessage = _.msg contains warn)
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

    val List(c, d) = compileClasses(code, allowMessage = _.msg contains warn)
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

    val List(c, d) = compileClasses(code)
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
        |  @inline final def m1a(f: Long => Int) = f(1L)
        |  def t1a = m1a(l => l.toInt) // after inlining m1a, we have the same situation as in t1
        |
        |  def t2 = {
        |    // there is no specialized variant of Function2 for this combination of types, so the IndyLambda has to create a generic Function2.
        |    // IndyLambda: SAM type is JFunction2, SAM is apply(ObjectObject)Object, body method is $anonfun$adapted(ObjectObject)Object
        |    val f = (b: Byte, i: Int) => i + b
        |    // invocation of apply(ObjectObject)Object, matches SAM in IndyLambda. arguments are boxed, result unboxed.
        |    f(1, 2)
        |    // opt: re-write to $anonfun$adapted
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
        |  def t4a = m4a((x: Int, y: Double) => 1L + x + y.toLong, 1, 2d) // IndyLambda uses specialized JFunction2$mcJID$sp. after inlining m4a, similar to t4.
        |
        |  def t5 = {
        |    // no specialization for the combination of primitives
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
        |  def t8 = m8((x: Int, y: Double) => 1L + x + y.toLong, 1, 2d)
        |  // opt: after inlining m8, rewrite to the body method $anonfun(ID)J, which requires inserting unbox operations for the params, box for the result
        |  // the box-unbox pairs can then be optimized away
        |
        |  // m9$mVc$sp invokes apply$mcVI$sp
        |  @inline final def m9[@specialized(Unit) U](f: Int => U): Unit = f(1)
        |  // IndyLambda: JFunction1, SAM is apply(Obj)Obj, body method $anonfun$adapted(Obj)Obj
        |  // invocation of m9$mVc$sp
        |  def t9 = m9(println)
        |  // opt: after inlining m9, rewrite to $anonfun$adapted(Obj)Obj, which requires inserting a box operation for the parameter.
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
    val List(c, _, _) = compileClasses(code)

    assertSameSummary(getMethod(c, "t1"), List(BIPUSH, ICONST_1, IADD, IRETURN))
    assertSameSummary(getMethod(c, "t1a"), List(LCONST_1, L2I, IRETURN))
    assertSameSummary(getMethod(c, "t2"), List(ICONST_1, ISTORE, ICONST_2, ILOAD, IADD, IRETURN))

    // val a = new ValKl(n); new ValKl(anonfun(a.x)).x
    // value class instantiation-extraction should be optimized by boxing elim
    assertSameSummary(getMethod(c, "t3"), List(
      NEW, DUP, ICONST_1, "<init>",
      "$anonfun$t3$1$adapted", CHECKCAST,
      "x", IRETURN))

    assertSameSummary(getMethod(c, "t4"), List(ICONST_1, "valueOf", ARETURN))
    assertSameSummary(getMethod(c, "t4a"), List(ICONST_1, ISTORE, LDC, DSTORE, LCONST_1, ILOAD, I2L, LADD, DLOAD, D2L, LADD, LRETURN))
    assertSameSummary(getMethod(c, "t5"), List(ICONST_1, "valueOf", ARETURN))
    assertSameSummary(getMethod(c, "t5a"), List(ICONST_1, IRETURN))
    assertSameSummary(getMethod(c, "t6"), List(RETURN))
    assertSameSummary(getMethod(c, "t7"), List(RETURN))
    assertSameSummary(getMethod(c, "t8"), List(ICONST_1, ISTORE, LDC, DSTORE, LCONST_1, ILOAD, I2L, LADD, DLOAD, D2L, LADD, LRETURN))
    assertSameSummary(getMethod(c, "t9"), List(ICONST_1, "valueOf", ASTORE, GETSTATIC, ALOAD, "println", RETURN))

    // t9a inlines Range.foreach, which is quite a bit of code, so just testing the core
    assertInvoke(getMethod(c, "t9a"), "java/lang/Integer", "valueOf")
    assertInvoke(getMethod(c, "t9a"), "java/lang/Integer", "valueOf")

    assertSameSummary(getMethod(c, "t10"), List(
      ICONST_1, ISTORE,
      ALOAD, ILOAD,
      "intCons", RETURN))

    // t10a inlines Range.foreach
    assertInvoke(getMethod(c, "t10a"), "C", "intCons")
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
        |    var x = -1                  // IntRef eliminated, heuristics inline anonfun method because
        |    m(i => if (i == 10) x = 1)  // it has an IntRef parameter
        |    x
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t1"), List(Op(ICONST_0), Op(ICONST_1), Op(IADD), Op(IRETURN)))
    assertNoInvoke(getMethod(c, "t2"))
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
    val c = compileClass(code)
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
        |  @inline final def asOs(a: Any) = a.asInstanceOf[Array[Object]]
        |  @inline final def asCs(a: Any) = a.asInstanceOf[Array[C]]
        |  @inline final def c0(a: Array[Object]) = a(0).asInstanceOf[C]
        |
        |  def t1(c: C) = asC(c) // eliminated
        |  def t2(c: C) = asO(c) // eliminated
        |  def t3(c: Object) = asC(c) // not eliminated
        |  def t4(c: C, d: D, b: Boolean) = asC(if (b) c else d) // not eliminated: lub of two non-equal reference types approximated with Object
        |  def t5(c: C, d: D, b: Boolean) = asO(if (b) c else d)
        |  def t6(c: C, cs: Array[C], b: Boolean) = asO(if (b) c else cs)
        |  def t7(a: Array[Int]) = asO(a)
        |  def t8(a: Array[Int]) = asC(a)
        |  def t9(a: Array[Int]) = asOs(a)
        |  def t10(a: Array[Object]) = asO(a)
        |  def t11(a: Array[Object]) = asOs(a)
        |  def t12(a: Array[Object]) = asCs(a)
        |  def t13(a: Array[C]) = c0(a.asInstanceOf[Array[Object]])
        |}
        |class D extends C
      """.stripMargin
    val List(c, _) = compileClasses(code)
    def casts(mn: String) = {
      val m = getMethod(c, mn)
      assertNoInvoke(m) // everything is inlined
      m.instructions collect { case TypeOp(CHECKCAST, tp) => tp }
    }
    assertSameCode(getMethod(c, "t1"), List(VarOp(ALOAD, 1), Op(ARETURN)))
    assertSameCode(getMethod(c, "t2"), List(VarOp(ALOAD, 1), Op(ARETURN)))
    assertSameCode(getMethod(c, "t3"), List(VarOp(ALOAD, 1), TypeOp(CHECKCAST, "C"), Op(ARETURN)))
    assertEquals(casts("t4"), List("C"))
    assertEquals(casts("t5"), Nil)
    assertEquals(casts("t6"), Nil)
    assertEquals(casts("t7"), Nil)
    assertEquals(casts("t8"), List("C"))
    assertEquals(casts("t9"), List("[Ljava/lang/Object;"))
    assertEquals(casts("t10"), Nil)
    assertEquals(casts("t11"), Nil)
    assertEquals(casts("t12"), List("[LC;"))
    assertEquals(casts("t13"), Nil)
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

    val cls = compileClasses(code)
    val test = findClass(cls, "Test$")
    assertSameSummary(getMethod(test, "f"), List(
      GETSTATIC, POP, NEW, "<init>",
      BIPUSH, ICONST_1, IADD, IRETURN))
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
    val c = compileClass(code)

    // box-unbox will clean it up
    assertSameSummary(getMethod(c, "t"), List(
      ALOAD, "$anonfun$t$1", IFEQ /*A*/,
      ICONST_1, IRETURN,
      -1 /*A*/, ICONST_2, IRETURN))
  }

  @Test
  def inlineProject(): Unit = {
    val codeA = "final class A { @inline def f = 1 }"
    val codeB = "class B { def t(a: A) = a.f }"
    // tests that no warning is emitted
    val List(a, b) = compileClassesSeparately(List(codeA, codeB), extraArgs = "-opt:l:inline -opt-inline-from:B -opt-warnings")
    assertInvoke(getMethod(b, "t"), "A", "f")
  }

  @Test
  def sd86(): Unit = {
    val code =
      """trait T1 { @inline def f = 999 }
        |trait T2 { self: T1 => @inline override def f = 1 } // note that f is not final
        |class C extends T1 with T2
      """.stripMargin
    val List(c, t1, t2) = compileClasses(code, allowMessage = _ => true)
    // we never inline into mixin forwarders, see scala-dev#259
    assertInvoke(getMethod(c, "f"), "T2", "f$")
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
    val c :: _ = compileClasses(code): @unchecked
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
    val c = compileClass(code)
    val t = getMethod(c, "t")
    assertNoIndy(t)
    assertInvoke(t, "C", "consume")
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
    val c :: _ = compileClassesSeparately(codes, extraArgs = compilerArgs): @unchecked
    assertNoInvoke(getMethod(c, "t"))
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
    val run = compiler.newRun()
    run.compileSources(List(makeSourceFile(code1, "A.scala"), makeSourceFile(code2, "B.scala")))
    val List(_, _, c) = readAsmClasses(getGeneratedClassfiles(global.settings.outputDirs.getSingleOutput.get))
    def is(name: String) = getMethod(c, name).instructions.filterNot(_.isInstanceOf[FrameEntry])

    assertSameCode(is("t1"), List(
      Label(0), LineNumber(12, Label(0)),
      VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "A", "fx", "()V", false),
      Op(ICONST_1), Op(IRETURN), Label(6)))

    assertSameCode(is("t2"), List(
      Label(0), LineNumber(3, Label(0)), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "B", "fx", "()V", false),
      Label(4), LineNumber(4, Label(4)), Op(ICONST_1), Label(7), LineNumber(13, Label(7)), Op(IRETURN), Label(10)))

    assertSameCode(is("t3"), List(
      Label(0), LineNumber(9, Label(0)), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "fx", "()V", false),
      Label(4), LineNumber(10, Label(4)), Op(ICONST_1), Label(7), LineNumber(14, Label(7)), Op(IRETURN), Label(10)))
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
    val List(c, t) = compileClasses(code)
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

    val c = compileClass(code)
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

  @Test
  def sd259(): Unit = {
    // - trait methods are not inlined into their static super accessors, and also not into mixin forwarders.
    // - inlining an invocation of a mixin forwarder also inlines the static accessor and the trait method body.
    val code =
      """trait T {
        |        def m1a = 1
        |  final def m1b = 1
        |
        |  @inline       def m2a = 2
        |  @inline final def m2b = 2
        |
        |        def m3a(f: Int => Int) = f(1)
        |  final def m3b(f: Int => Int) = f(1)
        |}
        |final class A extends T
        |class C {
        |  def t1(t: T) = t.m1a
        |  def t2(t: T) = t.m1b
        |  def t3(t: T) = t.m2a
        |  def t4(t: T) = t.m2b
        |  def t5(t: T) = t.m3a(x => x)
        |  def t6(t: T) = t.m3b(x => x)
        |
        |  def t7(a: A) = a.m1a
        |  def t8(a: A) = a.m1b
        |  def t9(a: A) = a.m2a
        |  def t10(a: A) = a.m2b
        |  def t11(a: A) = a.m3a(x => x)
        |  def t12(a: A) = a.m3b(x => x)
        |}
      """.stripMargin
    val warn = "T::m2a()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden."
    var count = 0
    val List(a, c, t) = compileClasses(code, allowMessage = i => {count += 1; i.msg contains warn})
    assert(count == 1)

    assertInvoke(getMethod(t, "m1a$"), "T", "m1a")
    assertInvoke(getMethod(t, "m1b$"), "T", "m1b")
    assertInvoke(getMethod(t, "m2a$"), "T", "m2a")
    assertInvoke(getMethod(t, "m2b$"), "T", "m2b")
    assertInvoke(getMethod(t, "m3a$"), "T", "m3a")
    assertInvoke(getMethod(t, "m3b$"), "T", "m3b")

    assertInvoke(getMethod(a, "m1a"), "T", "m1a$")
    assertInvoke(getMethod(a, "m1b"), "T", "m1b$")
    assertInvoke(getMethod(a, "m2a"), "T", "m2a$")
    assertInvoke(getMethod(a, "m2b"), "T", "m2b$")
    assertInvoke(getMethod(a, "m3a"), "T", "m3a$")
    assertInvoke(getMethod(a, "m3b"), "T", "m3b$")

    assertInvoke(getMethod(c, "t1"), "T", "m1a")
    assertNoInvoke(getMethod(c, "t2"))

    assertInvoke(getMethod(c, "t3"), "T", "m2a") // could not inline
    assertNoInvoke(getMethod(c, "t4"))

    assertInvoke(getMethod(c, "t5"), "T", "m3a") // could not inline
    assertNoInvoke(getMethod(c, "t6")) // both forwarders inlined, closure eliminated

    assertNoInvoke(getMethod(c, "t7"))
    assertNoInvoke(getMethod(c, "t8"))

    assertNoInvoke(getMethod(c, "t9"))
    assertNoInvoke(getMethod(c, "t10"))

    assertNoInvoke(getMethod(c, "t11")) // both forwarders inlined, closure eliminated
    assertNoInvoke(getMethod(c, "t12")) // both forwarders inlined, closure eliminated
  }

  @Test
  def sd259b(): Unit = {
    val code =
      """trait T {
        |  def get = 1
        |  @inline final def m = try { get } catch { case _: Throwable => 1 }
        |}
        |class A extends T
        |class C {
        |  def t(a: A) = 1 + a.m // cannot inline a try block onto a non-empty stack
        |}
      """.stripMargin
    val warn =
      """T::m()I is annotated @inline but could not be inlined:
        |The operand stack at the callsite in C::t(LA;)I contains more values than the
        |arguments expected by the callee T::m()I. These values would be discarded
        |when entering an exception handler declared in the inlined method.""".stripMargin
    val List(a, c, t) = compileClasses(code, allowMessage = _.msg contains warn)

    // inlining of m$ is rolled back, because <invokespecial T.m> is not legal in class C.
    assertInvoke(getMethod(c, "t"), "T", "m$")
  }

  @Test
  def sd259c(): Unit = {
    val code =
      """trait T {
        |  def bar = 1
        |  @inline final def m = {
        |    @noinline def impl = bar // private, non-static method
        |    impl
        |  }
        |}
        |class A extends T
        |class C {
        |  def t(a: A) = a.m
        |}
      """.stripMargin
    val warn =
      """T::m()I is annotated @inline but could not be inlined:
        |The callee T::m()I contains the instruction INVOKESPECIAL T.impl$1 ()I (itf)
        |that would cause an IllegalAccessError when inlined into class C.""".stripMargin
    val List(a, c, t) = compileClasses(code, allowMessage = _.msg contains warn)
    assertInvoke(getMethod(c, "t"), "T", "m$")
  }

  @Test
  def sd259d(): Unit = {
    val code =
      """trait T {
        |  @inline final def m = 1
        |}
        |class C extends T {
        |  def t = super.m // inline call to T.m$ here, we're not in the mixin forwarder C.m
        |}
      """.stripMargin
    val List(c, t) = compileClasses(code)
    assertNoInvoke(getMethod(c, "t"))
    assertInvoke(getMethod(c, "m"), "T", "m$")
  }

  @Test
  def sd350(): Unit = {
    val code =
      """trait T {
        |  @inline final def f = 1
        |  val x = f
        |}
      """.stripMargin
    val List(t) = compileClasses(code)
    val i = getMethod(t, "$init$")
    assertDoesNotInvoke(i, "f")
    assertInvoke(i, "T", "T$_setter_$x_$eq")
  }

  @Test
  def sd479_same_unit_inlining_line_number(): Unit = {
    val code =
      """class Test {
        |  @inline final def foo(b: Boolean): String = {
        |    "foo"
        |  }
        |
        |  def bar(a: AnyRef, b: Boolean): AnyRef = {
        |    foo(b); a.toString // line 7
        |  }
        |}
      """.stripMargin
    val List(t) = compileClasses(code)
    val i = getMethod(t, "bar")
    assertSameCode(i.instructions, List(Label(0), LineNumber(7, Label(0)), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;", false), Op(ARETURN), Label(5)))
  }

  @Test
  def multipleRounds(): Unit = {
    val code =
      """abstract class A {
        |  @inline def f = g
        |  def g: Int
        |}
        |final class C extends A {
        |  @inline def g = 1
        |  def t(c: C) = c.f
        |}
      """.stripMargin
    val List(a, c) = compileClasses(code)
    assertNoInvoke(getMethod(c, "t"))
  }

  @Test
  def multipleInlineRollback(): Unit = {
    val code =
      """final class A {
        |  @inline def f = {
        |    def g = 0
        |    g
        |  }
        |  @inline def m = 0
        |}
        |class C {
        |  def t(a: A) = {
        |    a.m // inlined
        |    a.f // rolled back
        |    a.m // inlined
        |  }
        |}
      """.stripMargin
    val warn =
      """A::f()I is annotated @inline but could not be inlined:
        |The callee A::f()I contains the instruction INVOKESTATIC A.g$1 ()I
        |that would cause an IllegalAccessError when inlined into class C""".stripMargin
    val List(a, c) = compileClasses(code, allowMessage = _.msg contains warn)
    assertSameSummary(getMethod(c, "t"), List(ALOAD, IFNONNULL /*6*/, ACONST_NULL, ATHROW, -1 /*6*/, ICONST_0, IRETURN))
  }

  @Test
  def recursiveInlineClosureRewrite(): Unit = {
    // first rec is inlined. then the closure invocation is re-written. this triggers another round of inlining.
    val code =
      """trait T {
        |  @inline final def rec(x: Int, f: Int => Int): Int = if (x == 0) 0 else f(x) + rec(x - 1, f)
        |}
        |class C extends T {
        |  def t = rec(10, x => x)
        |}
      """.stripMargin
    val List(c, t) = compileClasses(code)
    // rec is inlined once, the closure application is rewritten to the body method
    assertInvokedMethods(getMethod(c, "t"), List("T.rec"))
  }

  @Test
  def overriddenSuperNoInline(): Unit = {
    val code =
      """class A {
        |  def m(x: Int) = 1
        |}
        |class C extends A {
        |  @inline override final def m(x: Int) = 2
        |  def m = 1 + super.m(1)
        |}
      """.stripMargin
    val List(a, c) = compileClasses(code)
    val m = getAsmMethods(c, "m").find(_.desc == "()I").get
    assert(convertMethod(m).instructions.contains(Invoke(INVOKESPECIAL, "A", "m", "(I)I", itf = false)), AsmUtils.textify(m))
  }

  @Test
  def noInlineTemporaryIllegalInstructions(): Unit = {
    // The problem with this example was:
    //   1. `foo` is inlined into `m`. the inliner knows that this is illegal, so it saved the state
    //      before. `m` is pushed on the queue of methods for further inlining.
    //   2. `m` is inlined into `t`, so `t` also calls the private method `A.impl`
    //   3. At the end, `m` is reverted because the call to `impl` was not inlined in a later round
    // This left `t` with an illegal instruction
    // The fix: when inlining leaves a method with an illegal access instruction, continue inlining
    // into that method until the instructions are gone or the method is rolled back.
    val code =
      """class A {
        |  @noinline private def impl = 0
        |  // We don't mark `foo` @inline but rely on the heuristics. Marking it @inline
        |  // causes `makeNotPrivate` to be called on `impl` during SuperAccessors.
        |  final def foo(f: Int => Int) = impl
        |  def t = foo(x => x)
        |}
        |class C {
        |  @inline final def m(a: A) = a.foo(x => x)
        |  def t(a: A) = m(a)
        |}
      """.stripMargin
    val List(a, c) = compileClasses(code)
    assert((getAsmMethod(a, "impl").access & ACC_PRIVATE) != 0)
    assertInvoke(getMethod(a, "foo"), "A", "impl")
    assertInvoke(getMethod(a, "t"), "A", "impl")
    assertInvoke(getMethod(c, "m"), "A", "foo") // rolled back
    assertInvoke(getMethod(c, "t"), "A", "foo")
  }

  @Test
  def cleanArrayForeach(): Unit = {
    val code =
      """class C {
        |  def consume(i: Int): Unit = ()
        |  def t1(a: Array[Int]) = a.foreach(consume)
        |  def t2(a: Array[String]) = a.foreach(_.trim)
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t1"), List(
      ALOAD, ARRAYLENGTH, ISTORE, ICONST_0, ISTORE, // get length, init loop counter
      -1 /*8*/, ILOAD, ILOAD, IF_ICMPGE /*25*/,     // check loop condition
      ALOAD, ILOAD, IALOAD, ISTORE, ALOAD, ILOAD, "consume", // load element, store into local, call body
      IINC, GOTO /*7*/,    // increase loop counter, jump
      -1 /*26*/, RETURN))

    assertSameSummary(getMethod(c, "t2"), List(
      ALOAD, ARRAYLENGTH, ISTORE, ICONST_0, ISTORE,
      -1 /*8*/, ILOAD, ILOAD, IF_ICMPGE /*24*/,
      ALOAD, ILOAD, AALOAD, "trim", POP,
      IINC, GOTO /*8*/,
      -1 /*24*/, RETURN)
    )
  }

  @Test
  def cleanArrayMap(): Unit = {
    val code =
      """class C {
        |  def t1(a: Array[Int]) = a.map(_ + 1)
        |  def t2(a: Array[String]) = a.map(_.trim)
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameSummary(getMethod(c, "t1"), List(
      ALOAD, ARRAYLENGTH, ISTORE, ILOAD, NEWARRAY, ASTORE, ILOAD, ICONST_0, IF_ICMPLE /*37*/, ICONST_0, ISTORE, // init new array, loop counter
      -1 /*15*/, ILOAD, ILOAD, IF_ICMPGE /*37*/, // loop condition
      ALOAD, ILOAD, IALOAD, ICONST_1, IADD, ISTORE, // compute element
      ALOAD, ILOAD, ILOAD, IASTORE, // store element
      IINC, GOTO /*15*/, // increase counter, jump
      -1 /*37*/, ALOAD, ARETURN)
    )
    assertSameSummary(getMethod(c, "t2"), List(
      ALOAD, ARRAYLENGTH, ISTORE, ILOAD, ANEWARRAY, ASTORE, ILOAD, ICONST_0, IF_ICMPLE /*39*/, ICONST_0, ISTORE, // init new array, loop counter
      -1 /*15*/, ILOAD, ILOAD, IF_ICMPGE /*39*/, // loop condition
      ALOAD, ILOAD, AALOAD, "trim", ASTORE, ALOAD, ILOAD, ALOAD, AASTORE, ACONST_NULL, ASTORE, // compute and store element
      IINC, GOTO /*15*/, // increase counter, jump
      -1 /*39*/, ALOAD, ARETURN)
    )
  }

  @Test
  def cleanArrayOps(): Unit = {
    // These calls all optimize to while loops, no boxing
    val code =
      """class C {
        |  // test for `exists` also covers `forall`, `indexWhere`, `find` (they are implemented the same)
        |  def t1a(a: Array[Int]) = a.exists(_ == 0)
        |  def t1b(a: Array[String]) = a.exists(_ == "")
        |
        |  // also covers `fold`, `foldRight`
        |  def t2a(a: Array[Int]) = a.foldLeft(0)(_ + _)
        |  def t2b(a: Array[String]) = a.foldLeft(0)(_ + _.length)
        |
        |  // also covers `scan`, `scanRight`
        |  def t3a(a: Array[Int]) = a.scanLeft(0)(_ + _)
        |  def t3b(a: Array[String]) = a.scanLeft(0)(_ + _.length)
        |  def t3c(a: Array[String]) = a.scanLeft("")((_, s) => s.trim)
        |
        |  def t4a(a: Array[Int]) = a.mapInPlace(_ + 1)
        |  def t4b(a: Array[String]) = a.mapInPlace(_.trim)
        |
        |  def t5a(a: Array[Int]) = a.count(_ == 0)
        |  def t5b(a: Array[String]) = a.count(_ == "")
        |}
      """.stripMargin

    val c = compileClass(code)

    assertInvokedMethods(getMethod(c, "t1a"), List("C.$anonfun$t1a$1"))
    // anonfun not inlined, not considered trivial (too many instructions compared to num params)
    assertEquals(global.genBCode.postProcessor.backendUtils.looksLikeForwarderOrFactoryOrTrivial(getAsmMethod(c, "$anonfun$t1a$1"), "C", allowPrivateCalls = false), -1)
    assertInvokedMethods(getMethod(c, "t1b"), List("C.$anonfun$t1b$1"))

    assertInvokedMethods(getMethod(c, "t2a"), List("java/lang/NullPointerException.<init>"))
    assertInvokedMethods(getMethod(c, "t2b"), List("java/lang/NullPointerException.<init>", "java/lang/String.length"))

    assertInvokedMethods(getMethod(c, "t3a"), Nil)
    assertInvokedMethods(getMethod(c, "t3b"), List("java/lang/String.length"))
    assertInvokedMethods(getMethod(c, "t3c"), List("java/lang/String.trim"))

    assertInvokedMethods(getMethod(c, "t4a"), Nil)
    assertInvokedMethods(getMethod(c, "t4b"), List("java/lang/String.trim"))

    assertInvokedMethods(getMethod(c, "t5a"), List("C.$anonfun$t5a$1"))
    assertInvokedMethods(getMethod(c, "t5b"), List("C.$anonfun$t5b$1"))
  }

  @Test @Ignore
  def cleanArrayPartition(): Unit = {
    // need to
    //   - inline ArrayOps$.elemTag$extension
    //   - intrinsify x.getClass.getComponentType if x is a primitive array (rewrite to `GETSTATIC java/lang/Integer.TYPE`)
    //   - inline ArrayBuilder.make (heuristic: inline if classtag is known -- however, that only happens in local opt, during inlining it's not yet known)
    //   - make `GETSTATIC java/lang/Integer.TYPE` known non-null
    //   - constant-fold `java/lang/Integer.TYPE.equals(java/lang/Integer.TYPE)`
    //   - this should result in having an ArrayBuilder.ofInt, and we could skip boxing / unboxing
    //   - not necessary for ref arrays (there's no boxing. maybe casting, but that's much okisher)
    val code =
    """class C {
      |  def t1(a: Array[Int]) = a.partition(_ == 0)
      |  def t2(a: Array[String]) = a.partition(_ == "")
      |}
    """.stripMargin
    val c = compileClass(code)
    println(AsmUtils.textify(getAsmMethod(c, "t1")))
    println(AsmUtils.textify(getAsmMethod(c, "t2")))
  }

  // need more special handling by the optimizer
  //  - partition: see comment above in test case
  //  - filter: same as partition, goal eliminate boxing
  //  - reverse: should avoid boxing primitives
  //     - need to inline
  //     - need to intrinsify ClassTag(int[].class.getComponentType).newArray
  //  - groupBy: box elim by inlining tags/ArrayBuilder.make?
  //
  // ok without special casing
  //  - withFilter: probably nothing can be done here
  //  - slice should be ok without inlining, it does a type test on the array, so no boxing. todo: benchmark
  //  - takeWhile / dropWhile / span are fine, inline for the HOF, then call slice. add test
  //  - zip: nothing to optimize, will box into tuple anyway
  //
  // questionable
  //  - sorted: looks very bad for primitive arrays, creates an array with boxed values.
  //    just call Arrays.sort instead... should we deprecate the method? only use is sorting
  //    generic arrays.
  //
  //
  // TODO CHECK
  //  - flatMap, flatten, collect: maybe OK, no boxing because it's using arraycopy (addAll)???
  //  - appended, prepended, -(All): should be ok because using arraycopy?
  //  - contains: should probably @inline to elim boxing
  //  - patch: should be ok because using arraycopy
  //  - toArray, copyToArray, startsWith, endsWith: inline to avoid boxing? is there boxing? we end up in System.arraycopy anyway, so should be fine.
  //  - updated: @inline to avoid boxing?

  @Test
  def cleanRangeForeach(): Unit = {
    // IntRef is eliminated
    val code =
      """class C {
        |  def t = {
        |    var x = 0
        |    for (i <- 1 to 10) x += i
        |    x
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertInvokedMethods(getMethod(c, "t"), List(
      "scala/collection/immutable/Range$Inclusive.<init>",
      "scala/collection/immutable/Range.isEmpty",
      "scala/collection/immutable/Range.start",
      "scala/collection/immutable/Range.step")
    )
  }

  @Test
  def byteArrayMap(): Unit = {
    val code =
      """class C {
        |  def t1(a: Array[Byte]): Array[Int] = a map (_ + 1)
        |  def t2(a: Array[Byte]): Array[Byte] = a map (x => (x + 1).toByte)
        |}
      """.stripMargin
    val c = compileClass(code)
    def isArrOp(opc: Int): Boolean = opc >= IALOAD && opc <= SALOAD || opc >= IASTORE && opc <= SASTORE
    assertSameSummary(getInstructions(c, "t1").filter(i => isArrOp(i.opcode)), List(BALOAD, IASTORE))
    assertInvokedMethods(getMethod(c, "t1"), Nil)
    assertSameSummary(getInstructions(c, "t2").filter(i => isArrOp(i.opcode)), List(BALOAD, BASTORE))
    assertInvokedMethods(getMethod(c, "t2"), Nil)
  }

  @Test
  def noNewAliases(): Unit = {
    val code =
      """class C {
        |  @inline final def foo(x: Int, y: Long, z: Object) = 0
        |  def t(x: Int, y: Long, z: C) = foo(x, y, z) + { val yy = y; foo(x, yy, z) } + { val xx = x + 1; foo(xx, y, z) } + foo(x + 1, y, z.getClass) + z.foo(x, y, z)
        |}
      """.stripMargin
    val c = inlineOnlyCompiler.compileClass(code)
    val t = getMethod(c, "t")
    assertEquals(List("yy@5", "xx@7", "this@0", "x@1", "y@2", "z@4", "foo_x@11", "foo_z@12"), t.localVars.map(lv => s"${lv.name}@${lv.index}"))
    assertSameCode(t,
      List(
        // inlined first call
        VarOp(ALOAD, 0), VarOp(ILOAD, 1), VarOp(LLOAD, 2), VarOp(ALOAD, 4), Op(POP), Op(POP2), Op(POP), Op(POP), Op(ICONST_0),
        Jump(GOTO, Label(14)), Label(14),
        // store yy
        VarOp(LLOAD, 2), VarOp(LSTORE, 5),// store yy
        // inlined second call
        VarOp(ALOAD, 0), VarOp(ILOAD, 1), VarOp(LLOAD, 5), VarOp(ALOAD, 4), Op(POP), Op(POP2), Op(POP), Op(POP), Op(ICONST_0),
        // add
        Jump(GOTO, Label(32)), Label(32), Op(IADD),
        // store xx
        VarOp(ILOAD, 1), Op(ICONST_1), Op(IADD), VarOp(ISTORE, 7),
        VarOp(ALOAD, 0), VarOp(ILOAD, 7), VarOp(LLOAD, 2), VarOp(ALOAD, 4), Op(POP), Op(POP2), Op(POP), Op(POP), Op(ICONST_0),
        // add
        Jump(GOTO, Label(53)), Label(53), Op(IADD),
        // inlined third call
        VarOp(ALOAD, 0), VarOp(ILOAD, 1), Op(ICONST_1), Op(IADD), VarOp(LLOAD, 2), VarOp(ALOAD, 4), Invoke(INVOKEVIRTUAL, "C", "getClass", "()Ljava/lang/Class;", false), VarOp(ASTORE, 12), Op(POP2), VarOp(ISTORE, 11), Op(POP), Op(ICONST_0),
        // null out local, add
        Jump(GOTO, Label(72)), Label(72), Op(ACONST_NULL), VarOp(ASTORE, 12), Op(IADD),
        // inlined fourth call, with null test on parameter
        VarOp(ALOAD, 4), VarOp(ILOAD, 1), VarOp(LLOAD, 2), VarOp(ALOAD, 4), Op(POP), Op(POP2), Op(POP), Jump(IFNONNULL, Label(88)), Op(ACONST_NULL), Op(ATHROW), Label(88), Op(ICONST_0),
        // add
        Jump(GOTO, Label(93)), Label(93), Op(IADD),
        Op(IRETURN))
    )
  }

  @Test
  def nullOutLocals(): Unit = {
    val code =
      """class C {
        |  @inline final def foo(x: Int, a: String): String = { var x = "U"; if (x.hashCode > 0) x = a else return ""; x }
        |  def t = { val s = foo(0, "a"); println(s) }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertSameCode(getMethod(c, "t"), List(
      // store argument of inlined method in local
      Ldc(LDC, "a"), VarOp(ASTORE, 2),
      // x = U
      Ldc(LDC, "U"), VarOp(ASTORE, 3),
      // if (x.hashCode)
      VarOp(ALOAD, 3), Invoke(INVOKEVIRTUAL, "java/lang/String", "hashCode", "()I", false), Op(ICONST_0), Jump(IF_ICMPLE, Label(16)),
      // x = a
      VarOp(ALOAD, 2), VarOp(ASTORE, 3), Jump(GOTO, Label(20)),
      // res = ""
      Label(16), Ldc(LDC, ""), Jump(GOTO, Label(23)),
      // res = x
      Label(20), VarOp(ALOAD, 3),
      // arg-local, x = null
      Label(23), Op(ACONST_NULL), VarOp(ASTORE, 2), Op(ACONST_NULL), VarOp(ASTORE, 3), VarOp(ASTORE, 1),
      // println(s)
      Field(GETSTATIC, "scala/Console$", "MODULE$", "Lscala/Console$;"), VarOp(ALOAD, 1), Invoke(INVOKEVIRTUAL, "scala/Console$", "println", "(Ljava/lang/Object;)V", false), Op(RETURN))
    )
  }

  @Test
  def nullOutReturnLocal(): Unit = {
    val code =
      """class C {
        |  @noinline def mark(): Int = 1
        |  @inline final def f(a: Object, b: Object): Object = {
        |    val x = mark();
        |    if (x + { if (x == 0) return b else 1 } == 0) a
        |    else b
        |  }
        |  def t(a: Object, b: Object): Object = f(a, b)
        |}""".stripMargin
    val c = inlineOnlyCompiler.compileClass(code)
    assertSameCode(getMethod(c, "t").instructions.dropNonOp.dropWhile {
      case Invoke(_, _, "mark", _, _) => false
      case _ => true
    },
      List(
        Invoke(INVOKEVIRTUAL, "C", "mark", "()I", false),
        VarOp(ISTORE, 3),
        VarOp(ILOAD, 3),
        VarOp(ILOAD, 3),
        Op(ICONST_0),
        Jump(IF_ICMPNE, Label(25)),
        VarOp(ALOAD, 2),
        VarOp(ASTORE, 4),      // store result value in local variable
        Op(POP),               // pop the `x` from `x + ...` off the stack
        VarOp(ALOAD, 4),       // load the result
        Jump(GOTO, Label(38)), // jump to after-call
        Label(25),
        Op(ICONST_1),
        Op(IADD),
        Op(ICONST_0),
        Jump(IF_ICMPNE, Label(33)),
        VarOp(ALOAD, 1),
        Jump(GOTO, Label(38)),
        Label(33),
        VarOp(ALOAD, 2),
        Jump(GOTO, Label(38)),
        Label(38),             // after-call
        Op(ACONST_NULL),       // null out ...
        VarOp(ASTORE, 4),      // ... the return local variable
        Op(ARETURN),
      ))
  }

  @Test
  def arrayFill(): Unit = {
    val code =
      """class C {
        |  def t = Array.fill[Option[Any]](10)(None)
        |}
      """.stripMargin
    val c = compileClass(code)
    // no more calls
    assertInvokedMethods(getMethod(c, "t"), Nil)
    // no more casts / type tests (after inlining array_update)
    assertSameCode(
      getInstructions(c, "t").filter(_.isInstanceOf[TypeOp]),
      List(TypeOp(ANEWARRAY, "scala/Option"), TypeOp(ANEWARRAY, "scala/Option")))
  }

  @Test
  def noInlineGettersSupers(): Unit = {
    val code =
      """class A {
        |  @noinline def t2 = 0
        |}
        |final class C extends A {
        |  val field = 0
        |  def t1 = field
        |
        |  def hasSuper = super.t2
        |  override def t2 = hasSuper + 1
        |
        |  def hasPrivate = {
        |    @noinline def priv = field
        |    priv
        |  }
        |  def t3 = hasPrivate
        |
        |  def hasPublic = t1
        |  def t4 = hasPublic // inlined, we end up with t1 = field
        |
        |  def hasPrivateStatic = {
        |    @noinline def priv = 1
        |    priv // invokestatic of private method
        |  }
        |  def t5 = hasPrivateStatic
        |}
      """.stripMargin
    val List(a, c) = compileClasses(code)
    assertSameCode(getMethod(c, "t1"), List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "field", "()I", false), Op(IRETURN)))
    assertSameCode(getMethod(c, "t2"), List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "hasSuper", "()I", false), Op(ICONST_1), Op(IADD), Op(IRETURN)))
    assertEquals(getAsmMethod(c, "priv$1").access & (ACC_PRIVATE | ACC_STATIC), ACC_PRIVATE)
    assertSameCode(getMethod(c, "t3"), List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "hasPrivate", "()I", false), Op(IRETURN)))
    assertSameCode(getMethod(c, "t4"), List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "field", "()I", false), Op(IRETURN)))
    assertEquals(getAsmMethod(c, "priv$2").access & (ACC_PRIVATE | ACC_STATIC), ACC_PRIVATE | ACC_STATIC)
    assertSameCode(getMethod(c, "t5"), List(Invoke(INVOKESTATIC, "C", "priv$2", "()I", false), Op(IRETURN))) // TODO: should not inline here...
  }

  @Test
  def inlineSizeLimit(): Unit = {
    val code =
      """class C {
        |  @inline final def f = 0
        |  @inline final def g = f + f + f + f + f + f + f + f + f + f
        |  @inline final def h = g + g + g + g + g + g + g + g + g + g
        |  @inline final def i = h + h + h + h + h + h + h + h + h + h
        |  final def j = i + i + i
        |}
      """.stripMargin
    // Inlining stops when a method grows too large
    assertInvokedMethods(getMethod(compileClass(code), "j"),
      List("C.h", "C.h", "C.h", "C.h", "C.h", "C.h", "C.h", "C.i", "C.i"))
  }

  @Test
  def t11255(): Unit = {
    val codeA =
      """class K(val f: Int => Int) extends Serializable
        |class A {
        |  @inline final def f = new K(x => x + 1)
        |}
      """.stripMargin
    val codeB =
      """class C {
        |  def serializeDeserialize(obj: Object): Object = {
        |    import java.io._
        |    val buffer = new ByteArrayOutputStream
        |    val out = new ObjectOutputStream(buffer)
        |    out.writeObject(obj)
        |    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
        |    in.readObject
        |  }
        |
        |  def t = {
        |    serializeDeserialize((new A).f).asInstanceOf[K].f(10)
        |  }
        |}
      """.stripMargin
    val List(a, c, k) = compileClassesSeparately(List(codeA, codeB), extraArgs = "-opt:l:inline -opt-inline-from:**")
    val m = getMethod(c, "$deserializeLambda$")
    val args = m.instructions collect {
      case InvokeDynamic(opcode, name, desc, bsm, bsmArgs) =>
        val mh = bsmArgs.head.asInstanceOf[MethodHandle]
        List(mh.owner, mh.name)
    }
    assertEquals(List("A", "$anonfun$f$1"), args.head)
  }

  @Test
  def sd618(): Unit = {
    val code =
      """trait T {
        |  final def m1 = 1 // trivial
        |  final def m2 = p // forwarder
        |  @noinline def p = 42
        |}
        |
        |object TT extends T // gets mixin forwarders m1 / m2 which call the static T.m1$ / T.m2$
        |
        |class C {
        |  def t1a(t: T) = t.m1 // inlined, so we get 1
        |  def t1b = TT.m1      // mixin forwarder is inlined, static forwarder then as well because the final method is trivial
        |  def t2a(t: T) = t.m2 // inlined, so we get T.p
        |  def t2b = TT.m2      // mixin forwarder is inlined, static forwarder then as well because the final method is forwarder
        |}
      """.stripMargin
    val c :: _ = compileClasses(code): @unchecked
    assertNoInvoke(getMethod(c, "t1a"))
    assertNoInvoke(getMethod(c, "t1b"))
    assertInvoke(getMethod(c, "t2a"), "T", "p")
    assertInvoke(getMethod(c, "t2b"), "T", "p")
  }
}

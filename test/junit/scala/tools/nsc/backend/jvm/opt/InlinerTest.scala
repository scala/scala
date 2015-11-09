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
import scala.tools.nsc.reporters.StoreReporter

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._

import BackendReporting._

import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object InlinerTest extends ClearAfterClass.Clearable {
  val args = "-Yopt:l:classpath -Yopt-warnings"
  var compiler = newCompiler(extraArgs = args)
  var inlineOnlyCompiler = newCompiler(extraArgs = "-Yopt:inline-project")

  // allows inspecting the caches after a compilation run
  def notPerRun: List[Clearable] = List(
    compiler.genBCode.bTypes.classBTypeFromInternalName,
    compiler.genBCode.bTypes.byteCodeRepository.compilingClasses,
    compiler.genBCode.bTypes.byteCodeRepository.parsedClasses,
    compiler.genBCode.bTypes.callGraph.callsites)
  notPerRun foreach compiler.perRunCaches.unrecordCache

  def clear(): Unit = { compiler = null; inlineOnlyCompiler = null }
}

@RunWith(classOf[JUnit4])
class InlinerTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = InlinerTest

  val compiler = InlinerTest.compiler
  import compiler.genBCode.bTypes._
  import compiler.genBCode.bTypes.backendUtils._
  import inlinerHeuristics._

  val inlineOnlyCompiler = InlinerTest.inlineOnlyCompiler

  def compile(scalaCode: String, javaCode: List[(String, String)] = Nil, allowMessage: StoreReporter#Info => Boolean = _ => false): List[ClassNode] = {
    InlinerTest.notPerRun.foreach(_.clear())
    compileClasses(compiler)(scalaCode, javaCode, allowMessage)
    // Use the class nodes stored in the byteCodeRepository. The ones returned by compileClasses are not the same,
    // these are created new from the classfile byte array. They are completely separate instances which cannot
    // be used to look up methods / callsites in the callGraph hash maps for example.
    byteCodeRepository.compilingClasses.valuesIterator.toList.sortBy(_.name)
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
    val gMethod = findAsmMethod(c, "g")
    val fCall = getCallsite(gMethod, "f")
    (gMethod, fCall)
  }

  def canInlineTest(code: String, mod: ClassNode => Unit = _ => ()): Option[OptimizerWarning] = {
    val cs = gMethAndFCallsite(code, mod)._2
    inliner.earlyCanInlineCheck(cs) orElse inliner.canInlineBody(cs)
  }

  def inlineTest(code: String, mod: ClassNode => Unit = _ => ()): MethodNode = {
    val (gMethod, fCall) = gMethAndFCallsite(code, mod)
    inliner.inline(InlineRequest(fCall, Nil))
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

    assertSameCode(convertMethod(g).instructions.dropNonOp, gBeforeLocalOpt)

    compiler.genBCode.bTypes.localOpt.methodOptimizations(g, "C")
    assertSameCode(convertMethod(g).instructions.dropNonOp, invokeQQQ :+ Op(ATHROW))
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
      val f = cls.methods.asScala.find(_.name == "f").get
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
    val hMeth = findAsmMethod(d, "h")
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
    val instructions = getSingleMethod(cCls, "test").instructions
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
    val ins = getSingleMethod(c, "f").instructions
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
    val List(c, t, tClass) = compile(code)
    assertNoInvoke(getSingleMethod(c, "g"))
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
    assertNoInvoke(getSingleMethod(c, "g"))
  }

  @Test
  def inlineStaticCall(): Unit = {
    val code =
      """class C {
        |  def f = Integer.lowestOneBit(103)
        |}
      """.stripMargin

    val List(c) = compile(code)
    val fMeth = findAsmMethod(c, "f")
    val call = getCallsite(fMeth, "lowestOneBit")

    val warning = inliner.canInlineBody(call)
    assert(warning.isEmpty, warning)

    inliner.inline(InlineRequest(call, Nil))
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
    val List(c) = compileClasses(inlineOnlyCompiler)(code)
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
        |Note that the following parent classes are defined in Java sources (mixed compilation), no bytecode is available: A""".stripMargin

    var c = 0
    val List(b) = compile(scalaCode, List((javaCode, "A.java")), allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)
    val ins = getSingleMethod(b, "g").instructions
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
    val List(c, t, tClass) = compile(code)
    // both are just `return 1`, no more calls
    assertNoInvoke(getSingleMethod(c, "t1"))
    assertNoInvoke(getSingleMethod(c, "t2"))
  }

  @Test
  def inlineMixinMethods(): Unit = {
    val code =
      """trait T {
        |  @inline final def f = 1
        |}
        |class C extends T
      """.stripMargin
    val List(c, t, tClass) = compile(code)
    // the static implementation method is inlined into the mixin, so there's no invocation in the mixin
    assertNoInvoke(getSingleMethod(c, "f"))
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
    val List(c, t, tClass, u, uClass) = compile(code)
    assertNoInvoke(getSingleMethod(c, "t1"))
    assertNoInvoke(getSingleMethod(c, "t2"))
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
    val warns = Set(
      "C::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden",
      "T::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden")
    var count = 0
    val List(c, t, tClass) = compile(code, allowMessage = i => {count += 1; warns.exists(i.msg contains _)})
    assert(count == 2, count)
    assertInvoke(getSingleMethod(c, "t1"), "T", "f")
    assertInvoke(getSingleMethod(c, "t2"), "C", "f")
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
    val List(c, t, tClass) = compile(code)
    assertNoInvoke(getSingleMethod(c, "t1"))
  }

  @Test
  def inlineFromObject(): Unit = {
    val code =
      """trait T {
        |  @inline def f = 0
        |}
        |object O extends T {
        |  @inline def g = 1
        |  // mixin generates `def f = T$class.f(this)`, which is inlined here (we get ICONST_0)
        |}
        |class C {
        |  def t1 = O.f       // the mixin method of O is inlined, so we directly get the ICONST_0
        |  def t2 = O.g       // object members are inlined
        |  def t3(t: T) = t.f // no inlining here
        |}
      """.stripMargin
    val warn = "T::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden"
    var count = 0
    val List(c, oMirror, oModule, t, tClass) = compile(code, allowMessage = i => {count += 1; i.msg contains warn})
    assert(count == 1, count)

    assertNoInvoke(getSingleMethod(oModule, "f"))

    assertNoInvoke(getSingleMethod(c, "t1"))
    assertNoInvoke(getSingleMethod(c, "t2"))
    assertInvoke(getSingleMethod(c, "t3"), "T", "f")
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
        |  @inline final def n = m // inlined. (*)
        |  // (*) the declaration class of m is T. the signature of T$class.m is m(LAssembly;)I. so we need the self type to build the
        |  //     signature. then we can look up the MethodNode of T$class.m and then rewrite the INVOKEINTERFACE to INVOKESTATIC.
        |}
        |class C {
        |  def t1(a: Assembly) = a.f // like above, decl class is T, need self-type of T to rewrite the interface call to static.
        |  def t2(a: Assembly) = a.n
        |}
      """.stripMargin

    val List(assembly, assemblyClass, c, t, tClass) = compile(code)

    assertNoInvoke(getSingleMethod(tClass, "f"))

    assertNoInvoke(getSingleMethod(assemblyClass, "n"))

    assertNoInvoke(getSingleMethod(c, "t1"))
    assertNoInvoke(getSingleMethod(c, "t2"))
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
        |  @inline def g1 = f     // not inlined: f not final, so T1$class.g1 has an interface call T1.f
        |}
        |
        |// erased self-type (used in impl class for `self` parameter): T1
        |trait T2a { self: T1 with T2a =>
        |  @inline override final def f = 1
        |  @inline def g2a = f    // inlined: resolved as T2a.f, which is re-written to T2a$class.f, so T2a$class.g2a has ICONST_1
        |}
        |
        |final class Ca extends T1 with T2a {
        |  // mixin generates accessors like `def g1 = T1$class.g1`, the impl class method call is inlined into the accessor.
        |
        |  def m1a = g1           // call to accessor, inlined, we get the interface call T1.f
        |  def m2a = g2a          // call to accessor, inlined, we get ICONST_1
        |  def m3a = f            // call to accessor, inlined, we get ICONST_1
        |
        |  def m4a(t: T1) = t.f   // T1.f is not final, so not inlined, interface call to T1.f
        |  def m5a(t: T2a) = t.f  // re-written to T2a$class.f, inlined, ICONST_1
        |}
        |
        |// erased self-type: T2b
        |trait T2b { self: T2b with T1 =>
        |  @inline override final def f = 1
        |  @inline def g2b = f    // not inlined: resolved as T1.f, so T2b$class.g2b has an interface call T1.f
        |}
        |
        |final class Cb extends T1 with T2b {
        |  def m1b = g1           // inlined, we get the interface call to T1.f
        |  def m2b = g2b          // inlined, we get the interface call to T1.f
        |  def m3b = f            // inlined, we get ICONST_1
        |
        |  def m4b(t: T1) = t.f   // T1.f is not final, so not inlined, interface call to T1.f
        |  def m5b(t: T2b) = t.f  // re-written to T2b$class.f, inlined, ICONST_1
        |}
      """.stripMargin

    val warning = "T1::f()I is annotated @inline but cannot be inlined: the method is not final and may be overridden"
    var count = 0
    val List(ca, cb, t1, t1C, t2a, t2aC, t2b, t2bC) = compile(code, allowMessage = i => {count += 1; i.msg contains warning})
    assert(count == 4, count) // see comments, f is not inlined 4 times

    val t2aCfDesc = t2aC.methods.asScala.find(_.name == "f").get.desc
    assert(t2aCfDesc == "(LT1;)I", t2aCfDesc) // self-type of T2a is T1

    val t2bCfDesc = t2bC.methods.asScala.find(_.name == "f").get.desc
    assert(t2bCfDesc == "(LT2b;)I", t2bCfDesc) // self-type of T2b is T2b

    assertNoInvoke(getSingleMethod(t2aC, "g2a"))
    assertInvoke(getSingleMethod(t2bC, "g2b"), "T1", "f")

    assertInvoke(getSingleMethod(ca, "m1a"), "T1", "f")
    assertNoInvoke(getSingleMethod(ca, "m2a"))            // no invoke, see comment on def g2a
    assertNoInvoke(getSingleMethod(ca, "m3a"))
    assertInvoke(getSingleMethod(ca, "m4a"), "T1", "f")
    assertNoInvoke(getSingleMethod(ca, "m5a"))

    assertInvoke(getSingleMethod(cb, "m1b"), "T1", "f")
    assertInvoke(getSingleMethod(cb, "m2b"), "T1", "f")  // invoke, see comment on def g2b
    assertNoInvoke(getSingleMethod(cb, "m3b"))
    assertInvoke(getSingleMethod(cb, "m4b"), "T1", "f")
    assertNoInvoke(getSingleMethod(cb, "m5b"))
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
    assertNoInvoke(getSingleMethod(t, "t1"))
  }

  @Test
  def inlineFromNestedClasses(): Unit = {
    val code =
      """class C {
        |  trait T { @inline final def f = 1 }
        |  class D extends T{
        |    def m(t: T) = t.f
        |  }
        |
        |  def m(d: D) = d.f
        |}
      """.stripMargin
    val List(c, d, t, tC) = compile(code)
    assertNoInvoke(getSingleMethod(d, "m"))
    assertNoInvoke(getSingleMethod(c, "m"))
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
    val List(c, t, tc) = compile(code)
    val t1 = getSingleMethod(tc, "t1")
    val t2 = getSingleMethod(tc, "t2")
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
        |  final lazy val c = 2
        |  final val d = 3
        |  final val d1: Int = 3
        |
        |  @noinline def f = 5       // re-written to T$class
        |  @noinline final def g = 6 // re-written
        |
        |  @noinline def h: Int
        |  @inline def i: Int
        |}
        |
        |trait U { // not sealed
        |  lazy val a = 0
        |  val b = 1
        |  final lazy val c = 2
        |  final val d = 3
        |  final val d1: Int = 3
        |
        |  @noinline def f = 5       // not re-written (not final)
        |  @noinline final def g = 6 // re-written
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

    val List(c, t, tClass, u, uClass) = compile(code, allowMessage = _.msg contains "i()I is annotated @inline but cannot be inlined")
    val m1 = getSingleMethod(c, "m1")
    assertInvoke(m1, "T", "a")
    assertInvoke(m1, "T", "b")
    assertInvoke(m1, "T", "c")

    assertNoInvoke(getSingleMethod(c, "m2"))

    val m3 = getSingleMethod(c, "m3")
    assertInvoke(m3, "T$class", "f")
    assertInvoke(m3, "T$class", "g")
    assertInvoke(m3, "T", "h")
    assertInvoke(m3, "T", "i")

    val m4 = getSingleMethod(c, "m4")
    assertInvoke(m4, "U", "a")
    assertInvoke(m4, "U", "b")
    assertInvoke(m4, "U", "c")

    assertNoInvoke(getSingleMethod(c, "m5"))

    val m6 = getSingleMethod(c, "m6")
    assertInvoke(m6, "U", "f")
    assertInvoke(m6, "U$class", "g")
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
        |Note that the following parent classes could not be found on the classpath: A$Inner""".stripMargin

    var c = 0

    compileClasses(newCompiler(extraArgs = InlinerTest.args + " -Yopt-warnings:_"))(
      scalaCode,
      List((javaCode, "A.java")),
      allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)
  }

  @Test
  def inlineInvokeSpecial(): Unit = {
    val code =
      """class Aa {
        |  def f1 = 0
        |}
        |class B extends Aa {
        |  @inline final override def f1 = 1 + super.f1 // invokespecial Aa.f1
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
        |The callee B::f1()I contains the instruction INVOKESPECIAL Aa.f1 ()I
        |that would cause an IllegalAccessError when inlined into class T.""".stripMargin
    var c = 0
    val List(a, b, t) = compile(code, allowMessage = i => {c += 1; i.msg contains warn})
    assert(c == 1, c)

    assertInvoke(getSingleMethod(b, "t1"), "Aa", "f1")
    assertInvoke(getSingleMethod(b, "t2"), "B", "B$$f2m")
    assertInvoke(getSingleMethod(b, "t3"), "B", "<init>")
    assertInvoke(getSingleMethod(b, "t4"), "B", "<init>")

    assertInvoke(getSingleMethod(t, "t1"), "B", "f1")
    assertInvoke(getSingleMethod(t, "t2"), "B", "B$$f2m")
    assertInvoke(getSingleMethod(t, "t3"), "B", "<init>")
    assertInvoke(getSingleMethod(t, "t4"), "B", "<init>")
  }

  @Test
  def dontInlineNative(): Unit = {
    val code =
      """class C {
        |  def t = System.arraycopy(null, 0, null, 0, 0)
        |}
      """.stripMargin
    val List(c) = compileClasses(newCompiler(extraArgs = InlinerTest.args + " -Yopt-inline-heuristics:everything"))(code)
    assertInvoke(getSingleMethod(c, "t"), "java/lang/System", "arraycopy")
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
    assertInvoke(getSingleMethod(c, "t"), "java/lang/Error", "<init>")
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
    val t = getSingleMethod(c, "t").instructions
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

    val t1 = getSingleMethod(c, "t1")
    assert(t1.instructions forall { // indy is eliminated by push-pop
      case _: InvokeDynamic => false
      case _ => true
    })
    // the indy call is inlined into t, and the closure elimination rewrites the closure invocation to the body method
    assertInvoke(t1, "C", "C$$$anonfun$2")

    val t2 = getSingleMethod(c, "t2")
    assert(t2.instructions forall { // indy is eliminated by push-pop
      case _: InvokeDynamic => false
      case _ => true
    })
    assertInvoke(t2, "M$", "M$$$anonfun$1")
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
    val hMeth = findAsmMethod(c, "h")
    val gMeth = findAsmMethod(c, "g")
    val iMeth = findAsmMethod(c, "i")
    val fCall = getCallsite(gMeth, "f")
    val gCall = getCallsite(hMeth, "g")
    val hCall = getCallsite(iMeth, "h")

    val warning = inliner.canInlineBody(gCall)
    assert(warning.isEmpty, warning)

    inliner.inline(InlineRequest(hCall,
      post = List(InlineRequest(gCall,
        post = List(InlineRequest(fCall, Nil))))))
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
    val List(b, c, d) = List("b", "c", "d").map(findAsmMethod(cl, _))
    val aCall = getCallsite(b, "a")
    val bCall = getCallsite(c, "b")
    val cCall = getCallsite(d, "c")

    inliner.inline(InlineRequest(bCall, Nil))

    val req = InlineRequest(cCall,
      List(InlineRequest(bCall,
        List(InlineRequest(aCall, Nil)))))
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
    assertInvoke(getSingleMethod(c, "t1"), "C", "C$$$anonfun$1")
    assertInvoke(getSingleMethod(c, "t2"), "C", "a")
    assertInvoke(getSingleMethod(c, "t3"), "C", "b")
    assertNoInvoke(getSingleMethod(c, "t4"))
    assertNoInvoke(getSingleMethod(c, "t5"))
    assertNoInvoke(getSingleMethod(c, "t6"))
    assertInvoke(getSingleMethod(c, "t7"), "C", "c")
    assertInvoke(getSingleMethod(c, "t8"), "scala/Predef$", "println")
    assertNoInvoke(getSingleMethod(c, "t9"))
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
    assertNoInvoke(getSingleMethod(c, "t1"))
    assertInvoke(getSingleMethod(c, "t2"), "C", "f2")
    assertInvoke(getSingleMethod(c, "t3"), "C", "f1")
    assertInvoke(getSingleMethod(c, "t4"), "C", "f2")
    assertNoInvoke(getSingleMethod(c, "t5"))
    assertInvoke(getSingleMethod(c, "t6"), "C", "f3")
    assertNoInvoke(getSingleMethod(c, "t7"))
    assertInvoke(getSingleMethod(c, "t8"), "C", "f1")
    assertNoInvoke(getSingleMethod(c, "t9"))
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
    assertInvoke(getSingleMethod(c, "t1"), "C", "C$$$anonfun$1")
    assertInvoke(getSingleMethod(c, "t2"), "C", "C$$$anonfun$2")
    assertInvoke(getSingleMethod(c, "t3"), "scala/Function1", "apply$mcII$sp")
    assertInvoke(getSingleMethod(c, "t4"), "scala/Function1", "apply$mcII$sp")
    assertInvoke(getSingleMethod(c, "t5"), "C", "h")
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
    assertInvoke(getSingleMethod(c, "t"), "C", "g")
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
        |The callee C::h()I contains the instruction INVOKESPECIAL C.f$1 ()I
        |that would cause an IllegalAccessError when inlined into class D.""".stripMargin

    val List(c, d) = compile(code, allowMessage = _.msg contains warn)
    assertInvoke(getSingleMethod(c, "h"), "C", "f$1")
    assertInvoke(getSingleMethod(d, "t"), "C", "h")
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
    assertNoInvoke(getSingleMethod(c, "g"))
    assertNoInvoke(getSingleMethod(d, "t"))
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

    assertEquals(getSingleMethod(c, "t1").instructions.summary,
      List(BIPUSH, "C$$$anonfun$1", IRETURN))

    assertEquals(getSingleMethod(c, "t1a").instructions.summary,
      List(LCONST_1, "C$$$anonfun$2", IRETURN))

    assertEquals(getSingleMethod(c, "t2").instructions.summary, List(
      ICONST_1, ICONST_2, "C$$$anonfun$3",IRETURN))

    // val a = new ValKl(n); new ValKl(anonfun(a.x)).x
    // value class instantiation-extraction should be optimized by boxing elim
    assertEquals(getSingleMethod(c, "t3").instructions.summary, List(
      NEW, DUP, ICONST_1, "<init>", ASTORE,
      NEW, DUP, ALOAD, "x",
      "C$$$anonfun$4",
      "<init>",
      "x", IRETURN))

    assertEquals(getSingleMethod(c, "t4").instructions.summary, List(
      BIPUSH, "C$$$anonfun$5", "boxToInteger", ARETURN))

    assertEquals(getSingleMethod(c, "t4a").instructions.summary, List(
      ICONST_1, LDC, "C$$$anonfun$6", LRETURN))

    assertEquals(getSingleMethod(c, "t5").instructions.summary, List(
      BIPUSH, ICONST_3, "C$$$anonfun$7", "boxToInteger", ARETURN))

    assertEquals(getSingleMethod(c, "t5a").instructions.summary, List(
      BIPUSH, BIPUSH, I2B, "C$$$anonfun$8", IRETURN))

    assertEquals(getSingleMethod(c, "t6").instructions.summary, List(
      BIPUSH, "C$$$anonfun$9", RETURN))

    assertEquals(getSingleMethod(c, "t7").instructions.summary, List(
      ICONST_1, "C$$$anonfun$10", RETURN))

    assertEquals(getSingleMethod(c, "t8").instructions.summary, List(
      ICONST_1, LDC, "C$$$anonfun$11", LRETURN))

    assertEquals(getSingleMethod(c, "t9").instructions.summary, List(
      ICONST_1, "boxToInteger", "C$$$anonfun$12", RETURN))

    // t9a inlines Range.foreach, which is quite a bit of code, so just testing the core
    assertInvoke(getSingleMethod(c, "t9a"), "C", "C$$$anonfun$13")
    assert(getSingleMethod(c, "t9a").instructions.summary.contains("boxToInteger"))

    assertEquals(getSingleMethod(c, "t10").instructions.summary, List(
      ICONST_1, ISTORE,
      ALOAD, ILOAD,
      "C$$$anonfun$14", RETURN))

    // t10a inlines Range.foreach
    assertInvoke(getSingleMethod(c, "t10a"), "C", "C$$$anonfun$15")
    assert(!getSingleMethod(c, "t10a").instructions.summary.contains("boxToInteger"))
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
    assertSameCode(getSingleMethod(c, "t1").instructions.dropNonOp, List(Op(ICONST_0), Op(ICONST_1), Op(IADD), Op(IRETURN)))
    assertEquals(getSingleMethod(c, "t2").instructions collect { case i: Invoke => i.owner +"."+ i.name }, List(
      "scala/runtime/IntRef.create", "C.C$$$anonfun$1"))
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
    assertSameCode(getSingleMethod(c, "t1").instructions.dropNonOp, List(Op(ICONST_3), Op(ICONST_4), Op(IADD), Op(IRETURN)))
    assertSameCode(getSingleMethod(c, "t2").instructions.dropNonOp, List(Op(ICONST_1), Op(ICONST_2), Op(IADD), Op(IRETURN)))
    assertSameCode(getSingleMethod(c, "t3").instructions.dropNonOp, List(Op(ICONST_1), Op(ICONST_3), Op(ISUB), Op(IRETURN)))
    assertNoInvoke(getSingleMethod(c, "t4"))
    assertNoInvoke(getSingleMethod(c, "t5"))
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
    def casts(m: String) = getSingleMethod(c, m).instructions collect { case TypeOp(CHECKCAST, tp) => tp }
    assertSameCode(getSingleMethod(c, "t1").instructions.dropNonOp, List(VarOp(ALOAD, 1), Op(ARETURN)))
    assertSameCode(getSingleMethod(c, "t2").instructions.dropNonOp, List(VarOp(ALOAD, 1), Op(ARETURN)))
    assertSameCode(getSingleMethod(c, "t3").instructions.dropNonOp, List(VarOp(ALOAD, 1), TypeOp(CHECKCAST, "C"), Op(ARETURN)))
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
    val test = cls.find(_.name == "Test$").get
    assertEquals(
      getSingleMethod(test, "f").instructions.summary,
      List(GETSTATIC, "mkFoo",
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
    val t = getSingleMethod(c, "t")

    // box-unbox will clean it up
    assertEquals(getSingleMethod(c, "t").instructions.summary,
      List(
        ALOAD, "C$$$anonfun$1", IFEQ /*A*/,
        "C$$$anonfun$2", IRETURN,
        -1 /*A*/, "C$$$anonfun$3", IRETURN))
  }
}

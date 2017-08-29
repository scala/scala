package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.collection.immutable.IntMap
import scala.tools.asm.tree._
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class CallGraphTest extends BytecodeTesting {
  override def compilerArgs = "-opt:inline -opt-inline-from:** -opt-warnings"
  import compiler._
  import global.genBCode.{bTypes, postProcessor}
  import postProcessor.{byteCodeRepository, callGraph}


  compiler.keepPerRunCachesAfterRun(List(
    bTypes.classBTypeCacheFromSymbol,
    bTypes.classBTypeCacheFromClassfile,
    postProcessor.byteCodeRepository.compilingClasses,
    postProcessor.byteCodeRepository.parsedClasses,
    postProcessor.callGraph.callsites))

  import callGraph._
  import global.genBCode.bTypes._
  import postProcessor.bTypesFromClassfile._

  def callsInMethod(methodNode: MethodNode): List[MethodInsnNode] = methodNode.instructions.iterator.asScala.collect({
    case call: MethodInsnNode => call
  }).toList

  def checkCallsite(call: MethodInsnNode, callsiteMethod: MethodNode, target: MethodNode, calleeDeclClass: ClassBType,
                    safeToInline: Boolean, atInline: Boolean, atNoInline: Boolean, argInfos: IntMap[ArgInfo] = IntMap.empty) = {
    val callsite = callGraph.callsites(callsiteMethod)(call)
    try {
      assert(callsite.callsiteInstruction == call)
      assert(callsite.callsiteMethod == callsiteMethod)
      val callee = callsite.callee.get
      assert(callee.callee == target)
      assert(callee.calleeDeclarationClass == calleeDeclClass)
      assert(callee.safeToInline == safeToInline)
      assert(callee.annotatedInline == atInline)
      assert(callee.annotatedNoInline == atNoInline)
      assert(callsite.argInfos == argInfos)
    } catch {
      case e: Throwable => println(callsite); throw e
    }
  }

  @Test
  def callGraphStructure(): Unit = {
    val code =
      """class C {
        |  // try-catch prevents inlining - we want to analyze the callsite
        |  def f1                   = try { 0 } catch { case _: Throwable => 1 }
        |  final def f2             = try { 0 } catch { case _: Throwable => 1 }
        |
        |  @inline def f3           = try { 0 } catch { case _: Throwable => 1 }
        |  @inline final def f4     = try { 0 } catch { case _: Throwable => 1 }
        |
        |  @noinline def f5         = try { 0 } catch { case _: Throwable => 1 }
        |  @noinline final def f6   = try { 0 } catch { case _: Throwable => 1 }
        |
        |  @inline @noinline def f7 = try { 0 } catch { case _: Throwable => 1 } // no warning, @noinline takes precedence
        |}
        |class D extends C {
        |  @inline override def f1  = try { 0 } catch { case _: Throwable => 1 }
        |  override final def f3    = try { 0 } catch { case _: Throwable => 1 }
        |}
        |object C {
        |  def g1                   = try { 0 } catch { case _: Throwable => 1 }
        |}
        |class Test {
        |  def t1(c: C) = c.f1 + c.f2 + c.f3 + c.f4 + c.f5 + c.f6 + c.f7 + C.g1
        |  def t2(d: D) = d.f1 + d.f2 + d.f3 + d.f4 + d.f5 + d.f6 + d.f7 + C.g1
        |}
      """.stripMargin

    // Get the ClassNodes from the code repo (don't use the unparsed ClassNodes returned by compile).
    // The callGraph.callsites map is indexed by instructions of those ClassNodes.

    val ok = Set(
      "D::f1()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden.", // only one warning for D.f1: C.f1 is not annotated @inline
      "C::f3()I is annotated @inline but could not be inlined:\nThe method is not final and may be overridden.", // only one warning for C.f3: D.f3 does not have @inline (and it would also be safe to inline)
      "C::f4()I is annotated @inline but could not be inlined:\nThe operand stack at the callsite in Test::t1(LC;)I contains more values",
      "C::f4()I is annotated @inline but could not be inlined:\nThe operand stack at the callsite in Test::t2(LD;)I contains more values")
    var msgCount = 0
    val checkMsg = (m: StoreReporter#Info) => {
      msgCount += 1
      ok exists (m.msg contains _)
    }
    val List(cCls, cMod, dCls, testCls) = { compileClasses(code, allowMessage = checkMsg); compiledClassesFromCache }
    assert(msgCount == 4, msgCount)

    val List(cf1, cf2, cf3, cf4, cf5, cf6, cf7) = getAsmMethods(cCls, _.startsWith("f"))
    val List(df1, df3) = getAsmMethods(dCls, _.startsWith("f"))
    val g1 = getAsmMethod(cMod, "g1")
    val List(t1, t2) = getAsmMethods(testCls, _.startsWith("t"))

    val List(cf1Call, cf2Call, cf3Call, cf4Call, cf5Call, cf6Call, cf7Call, cg1Call) = callsInMethod(t1)
    val List(df1Call, df2Call, df3Call, df4Call, df5Call, df6Call, df7Call, dg1Call) = callsInMethod(t2)

    val cClassBType  = classBTypeFromClassNode(cCls)
    val cMClassBType = classBTypeFromClassNode(cMod)
    val dClassBType  = classBTypeFromClassNode(dCls)

    checkCallsite(cf1Call, t1, cf1, cClassBType, false, false, false)
    checkCallsite(cf2Call, t1, cf2, cClassBType, true, false, false)
    checkCallsite(cf3Call, t1, cf3, cClassBType, false, true, false)
    checkCallsite(cf4Call, t1, cf4, cClassBType, true, true, false)
    checkCallsite(cf5Call, t1, cf5, cClassBType, false, false, true)
    checkCallsite(cf6Call, t1, cf6, cClassBType, true, false, true)
    checkCallsite(cf7Call, t1, cf7, cClassBType, false, true, true)
    checkCallsite(cg1Call, t1, g1, cMClassBType, true, false, false)

    checkCallsite(df1Call, t2, df1, dClassBType, false, true, false)
    checkCallsite(df2Call, t2, cf2, cClassBType, true, false, false)
    checkCallsite(df3Call, t2, df3, dClassBType, true, false, false)
    checkCallsite(df4Call, t2, cf4, cClassBType, true, true, false)
    checkCallsite(df5Call, t2, cf5, cClassBType, false, false, true)
    checkCallsite(df6Call, t2, cf6, cClassBType, true, false, true)
    checkCallsite(df7Call, t2, cf7, cClassBType, false, true, true)
    checkCallsite(dg1Call, t2, g1, cMClassBType, true, false, false)
  }

  @Test
  def callerSensitiveNotSafeToInline(): Unit = {
    val code =
      """class C {
        |  def m = java.lang.Class.forName("C")
        |}
      """.stripMargin
    val List(c) = { compileClasses(code); compiledClassesFromCache }
    val m = getAsmMethod(c, "m")
    val List(fn) = callsInMethod(m)
    val forNameMeth = byteCodeRepository.methodNode("java/lang/Class", "forName", "(Ljava/lang/String;)Ljava/lang/Class;").get._1
    val classTp = cachedClassBType("java/lang/Class").get
    val r = callGraph.callsites(m)(fn)
    checkCallsite(fn, m, forNameMeth, classTp, safeToInline = false, atInline = false, atNoInline = false)
  }

  @Test
  def checkArgInfos(): Unit = {
    val code =
      """abstract class C {
        |  def h(f: Int => Int): Int = f(1)
        |  def t1 = h(x => x + 1)
        |  def t2(i: Int, f: Int => Int, z: Int) = h(f) + i - z
        |  def t3(f: Int => Int) = h(x => f(x + 1))
        |}
        |@FunctionalInterface trait D {
        |  def iAmASamD(x: Int): Int
        |  def selfSamCallD = iAmASamD(10)
        |}
        |trait E {
        |  def iAmASamE(x: Int): Int
        |  def selfSamCallE = iAmASamE(10)
        |}
        |""".stripMargin
    val List(c, d, e) = compileClasses(code)

    def callIn(m: String) = callGraph.callsites.find(_._1.name == m).get._2.values.head
    val t1h = callIn("t1")
    assertEquals(t1h.argInfos.toList, List((1, FunctionLiteral)))

    val t2h = callIn("t2")
    assertEquals(t2h.argInfos.toList, List((1, ForwardedParam(2))))

    val t3h = callIn("t3")
    assertEquals(t3h.argInfos.toList, List((1, FunctionLiteral)))

    val selfSamCallD = callIn("selfSamCallD")
    assertEquals(selfSamCallD.argInfos.toList, List((0,ForwardedParam(0))))

    val selfSamCallE = callIn("selfSamCallE")
    assertEquals(selfSamCallE.argInfos.toList, List())
  }

  @Test
  def argInfoAfterInlining(): Unit = {
    val code =
      """class C {
        |  def foo(f: Int => Int) = f(1)                 // not inlined
        |  @inline final def bar(g: Int => Int) = foo(g) // forwarded param 1
        |  @inline final def baz = foo(x => x + 1)       // literal
        |
        |  def t1 = bar(x => x + 1)                   // call to foo should have argInfo literal
        |  def t2(x: Int, f: Int => Int) = x + bar(f) // call to foo should have argInfo forwarded param 2
        |  def t3 = baz                               // call to foo should have argInfo literal
        |  def someFun: Int => Int = null
        |  def t4(x: Int) = x + bar(someFun)          // call to foo has empty argInfo
        |}
      """.stripMargin

    compileClasses(code)
    def callIn(m: String) = callGraph.callsites.find(_._1.name == m).get._2.values.head
    assertEquals(callIn("t1").argInfos.toList, List((1, FunctionLiteral)))
    assertEquals(callIn("t2").argInfos.toList, List((1, ForwardedParam(2))))
    assertEquals(callIn("t3").argInfos.toList, List((1, FunctionLiteral)))
    assertEquals(callIn("t4").argInfos.toList, Nil)
  }
}

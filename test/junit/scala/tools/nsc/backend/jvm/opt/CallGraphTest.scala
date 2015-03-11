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
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testing.AssertUtil._

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._
import BackendReporting._

import scala.collection.convert.decorateAsScala._

@RunWith(classOf[JUnit4])
class CallGraphTest {
  val compiler = newCompiler(extraArgs = "-Ybackend:GenBCode -Yopt:inline-global -Yopt-warnings")
  import compiler.genBCode.bTypes._

  // allows inspecting the caches after a compilation run
  val notPerRun: List[Clearable] = List(classBTypeFromInternalName, byteCodeRepository.classes, callGraph.callsites)
  notPerRun foreach compiler.perRunCaches.unrecordCache

  def compile(code: String, allowMessage: StoreReporter#Info => Boolean): List[ClassNode] = {
    notPerRun.foreach(_.clear())
    compileClasses(compiler)(code, allowMessage = allowMessage)
  }

  def callsInMethod(methodNode: MethodNode): List[MethodInsnNode] = methodNode.instructions.iterator.asScala.collect({
    case call: MethodInsnNode => call
  }).toList

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
        |  @inline @noinline def f7 = try { 0 } catch { case _: Throwable => 1 }
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
      "D::f1()I is annotated @inline but cannot be inlined: the method is not final and may be overridden", // only one warning for D.f1: C.f1 is not annotated @inline
      "C::f3()I is annotated @inline but cannot be inlined: the method is not final and may be overridden", // only one warning for C.f3: D.f3 does not have @inline (and it would also be safe to inline)
      "C::f7()I is annotated @inline but cannot be inlined: the method is not final and may be overridden", // two warnings (the error message mentions C.f7 even if the receiver type is D, because f7 is inherited from C)
      "operand stack at the callsite in Test::t1(LC;)I contains more values",
      "operand stack at the callsite in Test::t2(LD;)I contains more values")
    var msgCount = 0
    val checkMsg = (m: StoreReporter#Info) => {
      msgCount += 1
      ok exists (m.msg contains _)
    }
    val List(cCls, cMod, dCls, testCls) = compile(code, checkMsg).map(c => byteCodeRepository.classNode(c.name).get)
    assert(msgCount == 6, msgCount)

    val List(cf1, cf2, cf3, cf4, cf5, cf6, cf7) = cCls.methods.iterator.asScala.filter(_.name.startsWith("f")).toList.sortBy(_.name)
    val List(df1, df3) = dCls.methods.iterator.asScala.filter(_.name.startsWith("f")).toList.sortBy(_.name)
    val g1 = cMod.methods.iterator.asScala.find(_.name == "g1").get
    val List(t1, t2) = testCls.methods.iterator.asScala.filter(_.name.startsWith("t")).toList.sortBy(_.name)

    val List(cf1Call, cf2Call, cf3Call, cf4Call, cf5Call, cf6Call, cf7Call, cg1Call) = callsInMethod(t1)
    val List(df1Call, df2Call, df3Call, df4Call, df5Call, df6Call, df7Call, dg1Call) = callsInMethod(t2)

    def checkCallsite(callsite: callGraph.Callsite,
                      call: MethodInsnNode, callsiteMethod: MethodNode, target: MethodNode, calleeDeclClass: ClassBType,
                      safeToInline: Boolean, atInline: Boolean, atNoInline: Boolean) = try {
      assert(callsite.callsiteInstruction == call)
      assert(callsite.callsiteMethod == callsiteMethod)
      val callee = callsite.callee.get
      assert(callee.callee == target)
      assert(callee.calleeDeclarationClass == calleeDeclClass)
      assert(callee.safeToInline == safeToInline)
      assert(callee.annotatedInline == atInline)
      assert(callee.annotatedNoInline == atNoInline)

      assert(callsite.argInfos == List()) // not defined yet
    } catch {
      case e: Throwable => println(callsite); throw e
    }

    val cClassBType  = classBTypeFromClassNode(cCls)
    val cMClassBType = classBTypeFromClassNode(cMod)
    val dClassBType  = classBTypeFromClassNode(dCls)

    checkCallsite(callGraph.callsites(cf1Call),
      cf1Call, t1, cf1, cClassBType, false, false, false)
    checkCallsite(callGraph.callsites(cf2Call),
      cf2Call, t1, cf2, cClassBType, true, false, false)
    checkCallsite(callGraph.callsites(cf3Call),
      cf3Call, t1, cf3, cClassBType, false, true, false)
    checkCallsite(callGraph.callsites(cf4Call),
      cf4Call, t1, cf4, cClassBType, true, true, false)
    checkCallsite(callGraph.callsites(cf5Call),
      cf5Call, t1, cf5, cClassBType, false, false, true)
    checkCallsite(callGraph.callsites(cf6Call),
      cf6Call, t1, cf6, cClassBType, true, false, true)
    checkCallsite(callGraph.callsites(cf7Call),
      cf7Call, t1, cf7, cClassBType, false, true, true)
    checkCallsite(callGraph.callsites(cg1Call),
      cg1Call, t1, g1, cMClassBType, true, false, false)

    checkCallsite(callGraph.callsites(df1Call),
      df1Call, t2, df1, dClassBType, false, true, false)
    checkCallsite(callGraph.callsites(df2Call),
      df2Call, t2, cf2, cClassBType, true, false, false)
    checkCallsite(callGraph.callsites(df3Call),
      df3Call, t2, df3, dClassBType, true, false, false)
    checkCallsite(callGraph.callsites(df4Call),
      df4Call, t2, cf4, cClassBType, true, true, false)
    checkCallsite(callGraph.callsites(df5Call),
      df5Call, t2, cf5, cClassBType, false, false, true)
    checkCallsite(callGraph.callsites(df6Call),
      df6Call, t2, cf6, cClassBType, true, false, true)
    checkCallsite(callGraph.callsites(df7Call),
      df7Call, t2, cf7, cClassBType, false, true, true)
    checkCallsite(callGraph.callsites(dg1Call),
      dg1Call, t2, g1, cMClassBType, true, false, false)
  }
}

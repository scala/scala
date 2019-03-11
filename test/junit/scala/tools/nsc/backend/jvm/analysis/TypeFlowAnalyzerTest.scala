package scala.tools.nsc
package backend.jvm
package analysis

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Type
import scala.tools.asm.tree.analysis.BasicValue
import scala.tools.nsc.backend.jvm.analysis.TypeFlowInterpreter.{AaloadValue, LMFValue, ParamValue}
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

@RunWith(classOf[JUnit4])
class TypeFlowAnalyzerTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler._

  @Test
  def aaloadType(): Unit = {
    val m = compileAsmMethod("def f(a: Array[String]) = a(0)")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C")
    val v = a.frameAt(findInstr(m, "ARETURN")).getStack(0)
    assertEquals(v.getType, BasicValue.REFERENCE_VALUE.getType)
    assert(v.isInstanceOf[AaloadValue])
    assertEquals(a.preciseAaloadTypeDesc(v), "Ljava/lang/String;")
  }

  @Test
  def aaloadTypeMerge1(): Unit = {
    val m = compileAsmMethod("def f(a: Array[String], b: Array[String]) = { val x = if (a eq b) a else b; x(0) }")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C")
    val v = a.frameAt(findInstr(m, "ARETURN")).getStack(0)
    assertEquals(v.getType, BasicValue.REFERENCE_VALUE.getType)
    assert(v.isInstanceOf[AaloadValue])
    assertEquals(a.preciseAaloadTypeDesc(v), "Ljava/lang/String;")
  }

  @Test
  def aaloadTypeMerge2(): Unit = {
    val m = compileAsmMethod("def f(a: Array[String], b: Array[String]) = { (if (a eq b) a else b).apply(0) }")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C")
    val v = a.frameAt(findInstr(m, "ARETURN")).getStack(0)
    assertEquals(v.getType, BasicValue.REFERENCE_VALUE.getType)
    assert(v.isInstanceOf[AaloadValue])
    assertEquals(a.preciseAaloadTypeDesc(v), "Ljava/lang/String;")
  }

  @Test
  def aaloadTypeNoMerge(): Unit = {
    // two aaloads: the loaded type is `Object`
    val m = compileAsmMethod("def f(a: Array[String], b: Array[String]) = { if (a eq b) a(0) else b(0) }")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C")
    val v = a.frameAt(findInstr(m, "ARETURN")).getStack(0)
    assertEquals(v.getType, BasicValue.REFERENCE_VALUE.getType)
    assert(!v.isInstanceOf[AaloadValue])
    assertEquals(a.preciseAaloadTypeDesc(v), "Ljava/lang/Object;")
  }

  @Test
  def param(): Unit = {
    val m = compileAsmMethod("def f(a: String) = { val x = a; x.trim }")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C");
    {
      val v = a.frameAt(findInstr(m, "INVOKEVIRTUAL")).getStack(0)
      assertEquals(v.getType.getInternalName, "java/lang/String")
      assert(v.isInstanceOf[ParamValue])
    }
    {
      val v = a.frameAt(findInstr(m, "ARETURN")).getStack(0)
      assertEquals(v.getType.getInternalName, "java/lang/String")
      assert(!v.isInstanceOf[ParamValue])
    }
  }

  @Test
  def paramMerge(): Unit = {
    val m = compileAsmMethod("def f(a: String, b: String) = { val x = a; val y = a; (if (a eq b) x else y).trim }")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C");
    {
      val v = a.frameAt(findInstr(m, "INVOKEVIRTUAL")).getStack(0)
      assertEquals(v.getType.getInternalName, "java/lang/String")
      assert(v.isInstanceOf[ParamValue])
    }
    {
      val v = a.frameAt(findInstr(m, "ARETURN")).getStack(0)
      assertEquals(v.getType.getInternalName, "java/lang/String")
      assert(!v.isInstanceOf[ParamValue])
    }
  }

  @Test
  def paramNoMerge1(): Unit = {
    val m = compileAsmMethod("def f(a: String, b: String) = { val x = if (a eq b) a else b; x.trim }")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C");
    {
      val v = a.frameAt(findInstr(m, "INVOKEVIRTUAL")).getStack(0)
      assertEquals(v.getType.getInternalName, "java/lang/String")
      assert(!v.isInstanceOf[ParamValue])
    }
    {
      val v = a.frameAt(findInstr(m, "ARETURN")).getStack(0)
      assertEquals(v.getType.getInternalName, "java/lang/String")
      assert(!v.isInstanceOf[ParamValue])
    }
  }

  @Test
  def paramNoMerge2(): Unit = {
    val m = compileAsmMethod("def f(a: String, b: Object) = { val x = if (a eq b) a else b; x.hashCode }")
    val a = new NonLubbingTypeFlowAnalyzer(m, "C");
    {
      val v = a.frameAt(findInstr(m, "INVOKEVIRTUAL")).getStack(0)
      assertEquals(v.getType.getInternalName, "java/lang/Object")
      assert(!v.isInstanceOf[ParamValue])
    }
    {
      val v = a.frameAt(findInstr(m, "IRETURN")).getStack(0)
      assertEquals(v.getType, Type.INT_TYPE)
      assert(!v.isInstanceOf[ParamValue])
    }
  }

  @Test
  def lmf(): Unit = {
    val m = compileAsmMethods("def f = { val x = () => 1; x() }").find(_.name == "f").get
    val a = new NonLubbingTypeFlowAnalyzer(m, "C")
    val v = a.frameAt(findInstr(m, "INVOKEINTERFACE")).getStack(0)
    assertEquals(v.getType.getInternalName, "scala/runtime/java8/JFunction0$mcI$sp")
    assert(v.isInstanceOf[LMFValue])
  }

  @Test
  def lmfMerge(): Unit = {
    val m = compileAsmMethods("def f = { val x = if (this.hashCode == 0) () => 1 else () => 2; x() }").find(_.name == "f").get
    val a = new NonLubbingTypeFlowAnalyzer(m, "C")
    val v = a.frameAt(findInstr(m, "INVOKEINTERFACE")).getStack(0)
    assertEquals(v.getType.getInternalName, "scala/runtime/java8/JFunction0$mcI$sp")
    assert(v.isInstanceOf[LMFValue])
  }

  @Test
  def lmfNoMerge(): Unit = {
    val m = compileAsmMethods("def f = { val x = if (this.hashCode == 0) () => 1 else () => this; x() }").find(_.name == "f").get
    val a = new NonLubbingTypeFlowAnalyzer(m, "C")
    val v = a.frameAt(findInstr(m, "INVOKEINTERFACE")).getStack(0)
    assertEquals(v.getType.getInternalName, "java/lang/Object") // NonLubbingTypeFlowAnalyzer always sets lubs to Object
    assert(!v.isInstanceOf[LMFValue])
  }
}

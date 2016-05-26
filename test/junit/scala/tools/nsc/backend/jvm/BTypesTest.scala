package scala.tools.nsc
package backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class BTypesTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler.global
  locally {
    new global.Run() // initializes some of the compiler
    global.exitingDelambdafy(global.scalaPrimitives.init()) // needed: it's only done when running the backend, and we don't actually run the compiler
    global.exitingDelambdafy(global.genBCode.bTypes.initializeCoreBTypes())
  }
  import global.genBCode.bTypes._

  def classBTFS(sym: global.Symbol) = global.exitingDelambdafy(classBTypeFromSymbol(sym))

  def jlo = global.definitions.ObjectClass
  def jls = global.definitions.StringClass
  def o = classBTFS(jlo)
  def s = classBTFS(jls)
  def oArr = ArrayBType(o)
  def method = MethodBType(List(oArr, INT, DOUBLE, s), UNIT)

  @Test
  def classBTypesEquality() {
    val s1 = classBTFS(jls)
    val s2 = classBTFS(jls)
    val o  = classBTFS(jlo)
    assertEquals(s1, s2)
    assertEquals(s1.hashCode, s2.hashCode)
    assert(s1 != o)
    assert(s2 != o)
  }

  @Test
  def typedOpcodes() {
    assert(UNIT.typedOpcode(Opcodes.IALOAD)   == Opcodes.IALOAD)
    assert(INT.typedOpcode(Opcodes.IALOAD)    == Opcodes.IALOAD)
    assert(BOOL.typedOpcode(Opcodes.IALOAD)   == Opcodes.BALOAD)
    assert(BYTE.typedOpcode(Opcodes.IALOAD)   == Opcodes.BALOAD)
    assert(CHAR.typedOpcode(Opcodes.IALOAD)   == Opcodes.CALOAD)
    assert(SHORT.typedOpcode(Opcodes.IALOAD)  == Opcodes.SALOAD)
    assert(FLOAT.typedOpcode(Opcodes.IALOAD)  == Opcodes.FALOAD)
    assert(LONG.typedOpcode(Opcodes.IALOAD)   == Opcodes.LALOAD)
    assert(DOUBLE.typedOpcode(Opcodes.IALOAD) == Opcodes.DALOAD)
    assert(classBTFS(jls).typedOpcode(Opcodes.IALOAD) == Opcodes.AALOAD)

    assert(UNIT.typedOpcode(Opcodes.IRETURN)   == Opcodes.RETURN)
    assert(BOOL.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(CHAR.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(BYTE.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(SHORT.typedOpcode(Opcodes.IRETURN)  == Opcodes.IRETURN)
    assert(INT.typedOpcode(Opcodes.IRETURN)    == Opcodes.IRETURN)
    assert(FLOAT.typedOpcode(Opcodes.IRETURN)  == Opcodes.FRETURN)
    assert(LONG.typedOpcode(Opcodes.IRETURN)   == Opcodes.LRETURN)
    assert(DOUBLE.typedOpcode(Opcodes.IRETURN) == Opcodes.DRETURN)
    assert(classBTFS(jls).typedOpcode(Opcodes.IRETURN)   == Opcodes.ARETURN)
  }

  @Test
  def descriptors() {
    assert(o.descriptor == "Ljava/lang/Object;")
    assert(s.descriptor == "Ljava/lang/String;")
    assert(oArr.descriptor == "[Ljava/lang/Object;")
    assert(method.descriptor == "([Ljava/lang/Object;IDLjava/lang/String;)V")
  }

  @Test
  def toAsmTypeTest() {
    for (t <- List(o, s, oArr, method, INT, UNIT, DOUBLE)) {
      assertEquals(o.descriptor, o.toASMType.getDescriptor)
    }
  }

  // TODO @lry do more tests
  @Test
  def maxTypeTest() {

  }
}

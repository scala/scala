package scala.tools.nsc
package backend.jvm

import scala.tools.testing.AssertUtil._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.tools.asm.Opcodes
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class BTypesTest {
  val settings = new Settings()
  settings.processArgumentString("-usejavacp")
  val g: Global = new Global(settings)
  val run = new g.Run() // initializes some compiler internals
  import g.{definitions => d, Symbol}

  def duringBackend[T](f: => T) = g.exitingDelambdafy(f)

  val btypes = new BTypesFromSymbols[g.type](g)
  import btypes._
  duringBackend(btypes.intializeCoreBTypes())

  def classBTypeFromSymbol(sym: Symbol) = duringBackend(btypes.classBTypeFromSymbol(sym))

  val jlo = d.ObjectClass
  val jls = d.StringClass

  val o = classBTypeFromSymbol(jlo)
  val s = classBTypeFromSymbol(jls)
  val oArr = ArrayBType(o)
  val method = MethodBType(List(oArr, INT, DOUBLE, s), UNIT)

  @Test
  def classBTypesEquality() {
    val s1 = classBTypeFromSymbol(jls)
    val s2 = classBTypeFromSymbol(jls)
    val o  = classBTypeFromSymbol(jlo)
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
    assert(classBTypeFromSymbol(jls).typedOpcode(Opcodes.IALOAD) == Opcodes.AALOAD)

    assert(UNIT.typedOpcode(Opcodes.IRETURN)   == Opcodes.RETURN)
    assert(BOOL.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(CHAR.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(BYTE.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(SHORT.typedOpcode(Opcodes.IRETURN)  == Opcodes.IRETURN)
    assert(INT.typedOpcode(Opcodes.IRETURN)    == Opcodes.IRETURN)
    assert(FLOAT.typedOpcode(Opcodes.IRETURN)  == Opcodes.FRETURN)
    assert(LONG.typedOpcode(Opcodes.IRETURN)   == Opcodes.LRETURN)
    assert(DOUBLE.typedOpcode(Opcodes.IRETURN) == Opcodes.DRETURN)
    assert(classBTypeFromSymbol(jls).typedOpcode(Opcodes.IRETURN)   == Opcodes.ARETURN)
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

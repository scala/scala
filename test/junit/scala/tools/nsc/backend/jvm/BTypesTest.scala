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
  val g: Global = new Global(new Settings())

  val btypes = new BTypes[g.type](g) {
    def chrs = g.chrs
    override type BTypeName = g.TypeName
    override def createNewName(s: String) = g.newTypeName(s)
  }

  import btypes._

  val jls = "java/lang/String"
  val jlo = "java/lang/Object"

  val o = ClassBType(jlo)
  val s = ClassBType(jls)
  val oArr = ArrayBType(o)
  val method = MethodBType(List(oArr, INT, DOUBLE, s), UNIT)

  @Test
  def classBTypesEquality() {
    val s1 = ClassBType(jls)
    val s2 = ClassBType(jls)
    val o  = ClassBType(jlo)
    assertEquals(s1, s2)
    assertEquals(s1.hashCode, s2.hashCode)
    assert(s1 != o)
    assert(s2 != o)
  }

  @Test
  def classBTypeRequiresInternalName() {
    assertThrows[AssertionError](ClassBType(s"L$jls;"), _ contains "Descriptor instead of internal name")
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
    assert(ClassBType(jls).typedOpcode(Opcodes.IALOAD) == Opcodes.AALOAD)

    assert(UNIT.typedOpcode(Opcodes.IRETURN)   == Opcodes.RETURN)
    assert(BOOL.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(CHAR.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(BYTE.typedOpcode(Opcodes.IRETURN)   == Opcodes.IRETURN)
    assert(SHORT.typedOpcode(Opcodes.IRETURN)  == Opcodes.IRETURN)
    assert(INT.typedOpcode(Opcodes.IRETURN)    == Opcodes.IRETURN)
    assert(FLOAT.typedOpcode(Opcodes.IRETURN)  == Opcodes.FRETURN)
    assert(LONG.typedOpcode(Opcodes.IRETURN)   == Opcodes.LRETURN)
    assert(DOUBLE.typedOpcode(Opcodes.IRETURN) == Opcodes.DRETURN)
    assert(ClassBType(jls).typedOpcode(Opcodes.IRETURN)   == Opcodes.ARETURN)
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

  @Test
  def parseMethodDescriptorTest() {
    val descriptors = List(
      "()V",
      "(ID)I",
      "([[I[D)[D",
      s"(L$jls;[L$jlo;)[[L$jls;",
      s"(IL$jlo;)L$jls;"
    )
    for (d <- descriptors) {
      assertEquals(d, MethodBType(d).descriptor)
    }

    // class types in method descriptor need surrounding 'L' and ';'
    assertThrows[MatchError](MethodBType("(java/lang/String)V"), _ == "j (of class java.lang.Character)")
    assertThrows[AssertionError](MethodBType("I"), _ contains "Not a valid method descriptor")
  }
}

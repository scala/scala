package scala.tools.nsc
package backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable
import scala.tools.asm.Opcodes
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class BTypesTest extends BytecodeTesting {
  override def compilerArgs = "-opt:l:none"
  import compiler.global
  locally {
    new global.Run() // initializes some of the compiler
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

  def lazyLockForceTestCommon(doToString:Boolean, early:Boolean, withLock:Boolean): Unit = {
    val res = new mutable.StringBuilder()
    val l = if (withLock) Lazy.withLock({res append "forced;"; "VALUE"})
    else Lazy.withoutLock({res append "forced;"; "VALUE"})
    if (doToString) assertEquals("<?>", l.toString)
    assertEquals("", res.toString)

    if (early) assertEquals("VALUE", l.force)

    l.onForce(v => res append s"onF1:$v;")
    l.onForce(v => res append s"onF2:$v:${l.force};") // `force` within `onForce` returns the value

    var expValue = if (early) {
      "forced;onF1:VALUE;onF2:VALUE:VALUE;"
    } else {
      assertEquals("", res.toString)
      assertEquals("VALUE", l.force)
      "forced;onF2:VALUE:VALUE;onF1:VALUE;"
    }
    assertEquals(expValue, res.toString)
    l.onForce(v => res append s"onF3:$v;")
    expValue += "onF3:VALUE;"

    if (doToString) {
      //no effect from toString
      assertEquals("VALUE", l.toString)
      assertEquals(expValue, res.toString)
    }

    //reforcing should have no effect
    assertEquals("VALUE", l.force)
    assertEquals(expValue, res.toString)

    assertEquals("VALUE", l.toString)
    lazyLockAcquired(withLock)
  }
  def lazyLockAcquired(withLock:Boolean): Unit = {
    val res = new mutable.StringBuilder()
    val l = if (withLock) Lazy.withLock({res append s"forced:${Thread.holdsLock(frontendAccess.frontendLock)};"; "VALUE"})
    else Lazy.withoutLock({res append s"forced:${Thread.holdsLock(frontendAccess.frontendLock)};"; "VALUE"})

    l.onForce(v => res append s"onF1:$v:${Thread.holdsLock(frontendAccess.frontendLock)};")
    l.onForce(v => res append s"onF2:$v:${Thread.holdsLock(frontendAccess.frontendLock)};")
    assertEquals("", res.toString)

    l.force
    assertEquals(s"forced:$withLock;onF2:VALUE:false;onF1:VALUE:false;", res.toString)
    l.onForce(v => res append s"onF3:$v:${Thread.holdsLock(frontendAccess.frontendLock)};")
    assertEquals(s"forced:$withLock;onF2:VALUE:false;onF1:VALUE:false;onF3:VALUE:false;", res.toString)
  }

  @Test
  def lazyLockForceTest1(): Unit =
    lazyLockForceTestCommon(doToString = true, early = true, withLock = true)
  @Test
  def lazyLockForceTest2(): Unit =
    lazyLockForceTestCommon(doToString = false, early = true, withLock = true)
  @Test
  def lazyLockForceTest3(): Unit =
    lazyLockForceTestCommon(doToString = true, early = false, withLock = true)
  @Test
  def lazyLockForceTest4(): Unit =
    lazyLockForceTestCommon(doToString = false, early = false, withLock = true)

  @Test
  def lazyNoLockForceTest1(): Unit =
    lazyLockForceTestCommon(doToString = true, early = true, withLock = false)
  @Test
  def lazyNoLockForceTest2(): Unit =
    lazyLockForceTestCommon(doToString = false, early = true, withLock = false)
  @Test
  def lazyNoLockForceTest3(): Unit =
    lazyLockForceTestCommon(doToString = true, early = false, withLock = false)
  @Test
  def lazyNoLockForceTest4(): Unit =
    lazyLockForceTestCommon(doToString = false, early = false, withLock = false)

  def lazyEagerTestCommon(doToString:Boolean, early:Boolean): Unit = {
    val res = new mutable.StringBuilder()
    val l = Lazy.eager({res append "forced;"; "VALUE"})
    if (doToString) assertEquals("VALUE", l.toString)
    assertEquals("forced;", res.toString)

    if (early) assertEquals("VALUE", l.force)

    l.onForce(v => res append s"onF1:$v;")
    l.onForce(v => res append s"onF2:$v:${l.force};") // `force` within `onForce` returns the value

    var expValue = "forced;onF1:VALUE;onF2:VALUE:VALUE;"
    if (!early) {
      assertEquals(expValue, res.toString)
      assertEquals("VALUE", l.force)
    }
    assertEquals(expValue, res.toString)
    l.onForce(v => res append s"onF3:$v;")
    expValue += "onF3:VALUE;"

    if (doToString) {
      //no effect from toString
      assertEquals("VALUE", l.toString)
      assertEquals(expValue, res.toString)
    }

    //reforcing should have no effect
    assertEquals("VALUE", l.force)
    assertEquals(expValue, res.toString)

    assertEquals("VALUE", l.toString)
    lazyEagerAcquired()
  }
  def lazyEagerAcquired(): Unit = {
    val res = new mutable.StringBuilder()
    val l = Lazy.eager({res append s"forced:${Thread.holdsLock(frontendAccess.frontendLock)};"; "VALUE"})
    assertEquals(s"forced:false;", res.toString)
    l.onForce(v => res append s"onF1:$v:${Thread.holdsLock(frontendAccess.frontendLock)};")
    l.onForce(v => res append s"onF2:$v:${Thread.holdsLock(frontendAccess.frontendLock)};")
    assertEquals("forced:false;onF1:VALUE:false;onF2:VALUE:false;", res.toString)

    l.force
    assertEquals("forced:false;onF1:VALUE:false;onF2:VALUE:false;", res.toString)
    l.onForce(v => res append s"onF3:$v:${Thread.holdsLock(frontendAccess.frontendLock)};")
    assertEquals("forced:false;onF1:VALUE:false;onF2:VALUE:false;onF3:VALUE:false;", res.toString)
  }
  @Test
  def lazyEagerForceTest1(): Unit =
    lazyEagerTestCommon(doToString = true, early = true)
  @Test
  def lazyEagerForceTest2(): Unit =
    lazyEagerTestCommon(doToString = false, early = true)
  @Test
  def lazyEagerForceTest3(): Unit =
    lazyEagerTestCommon(doToString = true, early = false)
  @Test
  def lazyEagerForceTest4(): Unit =
    lazyEagerTestCommon(doToString = false, early = false)


  // TODO @lry do more tests
  @Test
  def maxTypeTest() {

  }
}

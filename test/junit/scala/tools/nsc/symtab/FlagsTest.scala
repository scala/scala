package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class FlagsTest extends BytecodeTesting {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  import Flags._

  def sym = NoSymbol.newTermSymbol(nme.EMPTY)

  def withFlagMask[A](mask: Long)(body: => A): A = enteringPhase(new Phase(NoPhase) {
    override def flagMask = mask
    def name = ""
    def run() = ()
  })(body)

  def testTimedFlag(flag: Long, test: Symbol => Boolean, enabling: Boolean) = {
    assertEquals(withFlagMask(InitialFlags)(test(sym.setFlag(flag))), !enabling)
    assertEquals(withFlagMask(InitialFlags | flag)(test(sym.setFlag(flag))), enabling)
  }

  def testLate(flag: Long, test: Symbol => Boolean) = testTimedFlag(flag, test, enabling = true)
  def testNot(flag: Long, test: Symbol => Boolean) = testTimedFlag(flag, test, enabling = false)

  @Test
  def testTimedFlags(): Unit = {
    testNot(PROTECTED | notPROTECTED, _.isProtected)
    testNot(PRIVATE | notPRIVATE, _.isPrivate)

    assertFalse(withFlagMask(AllFlags)(sym.setFlag(PRIVATE | notPRIVATE).isPrivate))

    assertEquals(withFlagMask(InitialFlags)(sym.setFlag(PRIVATE | notPRIVATE).flags & PRIVATE), PRIVATE)
    assertEquals(withFlagMask(AllFlags)(sym.setFlag(PRIVATE | notPRIVATE).flags & PRIVATE), 0)
  }

  @Test
  def normalLateOverlap(): Unit = {
    // late flags are shifted by LateShift == 47.
    // however, the first late flag is lateDEFERRED, which is DEFERRED << 47 == (1 << 4) << 47 == 1 << 51
    // the flags from 1 << 47 to 1 << 50 are not late flags. this is ensured by the LateFlags mask.

    for (i <- 0 to 3) {
      val f = 1L << i
      assertEquals(withFlagMask(AllFlags)(sym.setFlag(f << LateShift).flags & f), 0) // not treated as late flag
    }
    for (i <- 4 to 8) {
      val f = 1L << i
      assertEquals(withFlagMask(AllFlags)(sym.setFlag(f << LateShift).flags & f), f) // treated as late flag
    }
  }

  @Test
  def normalAnti(): Unit = {
    for (i <- 0 to 2) {
      val f = 1L << i
      assertEquals(withFlagMask(AllFlags)(sym.setFlag(f | (f << AntiShift)).flags & f), 0) // negated flags
    }
    for (i <- 3 to 7) {
      val f = 1L << i
      assertEquals(withFlagMask(AllFlags)(sym.setFlag(f | (f << AntiShift)).flags & f), f) // not negated
    }
  }

  @Test
  def lateAntiCrossCheck(): Unit = {
    val allButNegatable = AllFlags & ~(PROTECTED | OVERRIDE | PRIVATE)
    val lateable        = 0L | DEFERRED | FINAL | INTERFACE | METHOD | MODULE
    val lateFlags       = lateable << LateShift
    val allButLateable  = AllFlags & ~lateable

    assertEquals(withFlagMask(AllFlags)(sym.setFlag(AllFlags).flags), allButNegatable)
    assertEquals(withFlagMask(AllFlags)(sym.setFlag(allButLateable).flags), allButNegatable)

    assertEquals(withFlagMask(AllFlags)(sym.setFlag(lateFlags).flags), lateFlags | lateable)
  }

  @Test
  def javaClassMirrorAnnotationFlag(): Unit = {
    import scala.reflect.runtime.universe._
    val dep = typeOf[java.lang.Deprecated].typeSymbol
    assertTrue(dep.isJavaAnnotation && dep.isJava)
  }

  @Test
  def interfaceFlag(): Unit = {
    // scala traits are `isInterface` if they have only type defs and abstract methods / fields.
    // java interfaces are always `isInterface`.
    val scalaCode =
      """package p
        |trait T1 {
        |  import scala.collection
        |  def m: Int
        |  val f: Int
        |  type T <: AnyRef
        |}
        |trait T2 {
        |  def m = 1
        |}
        |trait T3 {
        |  val f = 1
        |}
        |trait T4 {
        |  println()
        |}
      """.stripMargin
    val javaI1 = "package p; interface I1 { int m(); }"
    val javaI2 = "package p; interface I2 { default int m() { return 1; } }"
    compiler.compileClasses(code = scalaCode, javaCode = (javaI1, "I1.java") :: (javaI2, "I2.java") :: Nil)
    import compiler.global.rootMirror._
    assert( getRequiredClass("p.T1").isInterface)
    assert(!getRequiredClass("p.T2").isInterface)
    assert(!getRequiredClass("p.T3").isInterface)
    assert(!getRequiredClass("p.T4").isInterface)
    assert( getRequiredClass("p.I1").isInterface)
    assert( getRequiredClass("p.I2").isInterface)
  }
}

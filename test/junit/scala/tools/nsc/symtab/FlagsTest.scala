package scala.tools.nsc
package symtab

import org.junit.Assert._
import scala.tools.testing.AssertUtil._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class FlagsTest {
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
    testLate(lateDEFERRED, _.isDeferred)
    testLate(lateFINAL, _.isFinal)
    testLate(lateINTERFACE, _.isInterface)
    testLate(lateMETHOD, _.isMethod)
    testLate(lateMODULE, _.isModule)
    testNot(PROTECTED | notPROTECTED, _.isProtected)
    testNot(OVERRIDE | notOVERRIDE, _.isOverride)
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
}

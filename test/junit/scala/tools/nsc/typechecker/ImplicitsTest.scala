package scala.tools.nsc
package typechecker

import org.junit.Assert.{assertEquals, assertNotEquals, assertTrue}
import org.junit.Test

import scala.tools.testkit.BytecodeTesting

class ImplicitsTest extends BytecodeTesting {
  import compiler.global._, definitions._, analyzer._

  @Test
  def implicitInfoHashCode(): Unit = {
    val run = new global.Run

    enteringPhase(run.typerPhase) {
      val T0 = IntClass.tpeHK
      val T1 = refinedType(List(T0), NoSymbol)

      assertTrue(T0 =:= T1)
      assertNotEquals(T0, T1)
      assertNotEquals(T0.hashCode, T1.hashCode)

      val I0 = new ImplicitInfo(TermName("dummy"), T0, NoSymbol)
      val I1 = new ImplicitInfo(TermName("dummy"), T1, NoSymbol)

      assertEquals(I0, I1)
      assertEquals(I0.hashCode, I1.hashCode)

      def implicitInfoHash(name: TermName, sym: Symbol) = {
        import scala.util.hashing.MurmurHash3._
        finalizeHash(mix(mix(productSeed, name.##), sym.##), 2)
      }

      val pHash = implicitInfoHash(TermName("dummy"), NoSymbol)

      assertEquals(I0.hashCode, pHash)
      assertEquals(I1.hashCode, pHash)
    }
  }
}

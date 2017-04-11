package scala.tools.nsc
package typechecker

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class ImplicitsTests extends BytecodeTesting {
  import compiler.global._, definitions._, analyzer._

  @Test
  def implicitInfoHashCode(): Unit = {
    val run = new global.Run

    enteringPhase(run.typerPhase) {
      val T0 = IntClass.tpeHK
      val T1 = refinedType(List(T0), NoSymbol)

      assert(T0 =:= T1)
      assert(T0 != T1)
      assert(T0.hashCode != T1.hashCode)

      val I0 = new ImplicitInfo(TermName("dummy"), T0, NoSymbol)
      val I1 = new ImplicitInfo(TermName("dummy"), T1, NoSymbol)

      assert(I0 == I1)
      assert(I0.hashCode == I1.hashCode)

      val pHash = (TermName("dummy"), NoSymbol).hashCode

      assert(I0.hashCode == pHash)
      assert(I1.hashCode == pHash)
    }
  }
}

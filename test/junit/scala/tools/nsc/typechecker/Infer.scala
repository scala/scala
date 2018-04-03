package scala.tools.nsc
package typechecker

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.BytecodeTesting

class A
class B extends A

trait C

trait X[+T]
trait Y[T]
trait Z[-T]
trait Quux[-T] extends Z[T]

trait SubZB extends Z[B]
trait ZwC[-T] extends Z[T] with C

@RunWith(classOf[JUnit4])
class InferencerTests extends BytecodeTesting {
  import compiler.global._, definitions._, analyzer._

  @Test
  def isAsSpecificDotty(): Unit = {
    val run = new global.Run

    enteringPhase(run.typerPhase) {
      val XA = typeOf[X[A]]
      val XB = typeOf[X[B]]

      val YA = typeOf[Y[A]]
      val YB = typeOf[Y[B]]

      val ZA = typeOf[Z[A]]
      val ZB = typeOf[Z[B]]

      val LZA = typeOf[List[Z[A]]]
      val LZB = typeOf[List[Z[B]]]

      val QuuxA = typeOf[Quux[A]]
      val QuuxB = typeOf[Quux[B]]

      val SubZB = typeOf[SubZB]
      val ZBwC = typeOf[ZwC[B]]

      // https://github.com/scala/bug/issues/2509
      // See discussion at https://github.com/lampepfl/dotty/blob/89540268e6c49fb92b9ca61249e46bb59981bf5a/src/dotty/tools/dotc/typer/Applications.scala#L925-L951

      // Covariant
      assert(!typer.infer.isAsSpecific(XA, XB, nullaryImplicitArgs = false))
      assert(typer.infer.isAsSpecific(XB, XA, nullaryImplicitArgs = false))

      // Invariant
      assert(!typer.infer.isAsSpecific(YA, YB, nullaryImplicitArgs = false))
      assert(!typer.infer.isAsSpecific(YB, YA, nullaryImplicitArgs = false))

      // Contravariant: treat top level as covariant
      assert(!typer.infer.isAsSpecific(ZA, ZB, nullaryImplicitArgs = false))
      assert(typer.infer.isAsSpecific(ZB, ZA, nullaryImplicitArgs = false))

      // Inner contravariant: unchanged
      assert(typer.infer.isAsSpecific(LZA, LZB, nullaryImplicitArgs = false))
      assert(!typer.infer.isAsSpecific(LZB, LZA, nullaryImplicitArgs = false))

      // Subtypes of contravaraint effectively unrelated
      assert(!typer.infer.isAsSpecific(ZA, SubZB, nullaryImplicitArgs = false))
      assert(typer.infer.isAsSpecific(SubZB, ZA, nullaryImplicitArgs = false))

      // Subtypes of contravaraint effectively unrelated
      assert(!typer.infer.isAsSpecific(ZA, ZBwC, nullaryImplicitArgs = false))
      assert(typer.infer.isAsSpecific(ZBwC, ZA, nullaryImplicitArgs = false))

      // Contravariant outer subtypes
      assert(!typer.infer.isAsSpecific(ZA, QuuxA, nullaryImplicitArgs = false))
      assert(typer.infer.isAsSpecific(QuuxA, ZA, nullaryImplicitArgs = false))

      assert(!typer.infer.isAsSpecific(ZA, QuuxB, nullaryImplicitArgs = false))
      assert(typer.infer.isAsSpecific(QuuxB, ZA, nullaryImplicitArgs = false))

      assert(!typer.infer.isAsSpecific(ZB, QuuxA, nullaryImplicitArgs = false))
      assert(!typer.infer.isAsSpecific(QuuxA, ZB, nullaryImplicitArgs = false))

      assert(!typer.infer.isAsSpecific(ZB, QuuxB, nullaryImplicitArgs = false))
      assert(typer.infer.isAsSpecific(QuuxB, ZB, nullaryImplicitArgs = false))

      // Covariant
      assert(!typer.infer.isAsSpecific(XA, XB, nullaryImplicitArgs = true))
      assert(typer.infer.isAsSpecific(XB, XA, nullaryImplicitArgs = true))

      // Invariant
      assert(!typer.infer.isAsSpecific(YA, YB, nullaryImplicitArgs = true))
      assert(!typer.infer.isAsSpecific(YB, YA, nullaryImplicitArgs = true))

      // Contravariant: treat top level as covariant
      assert(!typer.infer.isAsSpecific(ZA, ZB, nullaryImplicitArgs = true))
      assert(typer.infer.isAsSpecific(ZB, ZA, nullaryImplicitArgs = true))

      // Inner contravariant: unchanged
      assert(typer.infer.isAsSpecific(LZA, LZB, nullaryImplicitArgs = true))
      assert(!typer.infer.isAsSpecific(LZB, LZA, nullaryImplicitArgs = true))

      // Subtypes of contravaraint effectively unrelated
      assert(!typer.infer.isAsSpecific(ZA, SubZB, nullaryImplicitArgs = true))
      assert(typer.infer.isAsSpecific(SubZB, ZA, nullaryImplicitArgs = true))

      // Subtypes of contravaraint effectively unrelated
      assert(!typer.infer.isAsSpecific(ZA, ZBwC, nullaryImplicitArgs = true))
      assert(typer.infer.isAsSpecific(ZBwC, ZA, nullaryImplicitArgs = true))

      // Contravariant outer subtypes
      assert(!typer.infer.isAsSpecific(ZA, QuuxA, nullaryImplicitArgs = true))
      assert(typer.infer.isAsSpecific(QuuxA, ZA, nullaryImplicitArgs = true))

      assert(!typer.infer.isAsSpecific(ZA, QuuxB, nullaryImplicitArgs = true))
      assert(typer.infer.isAsSpecific(QuuxB, ZA, nullaryImplicitArgs = true))

      assert(!typer.infer.isAsSpecific(ZB, QuuxA, nullaryImplicitArgs = true))
      assert(!typer.infer.isAsSpecific(QuuxA, ZB, nullaryImplicitArgs = true))

      assert(!typer.infer.isAsSpecific(ZB, QuuxB, nullaryImplicitArgs = true))
      assert(typer.infer.isAsSpecific(QuuxB, ZB, nullaryImplicitArgs = true))
    }
  }
}

package scala.tools.nsc
package typechecker

import org.junit.Assert.assertTrue
import org.junit.{After, Before, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.settings.ScalaVersion
import scala.tools.testkit.BytecodeTesting

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
  import compiler.global._, analyzer._

  var storedXsource: ScalaVersion = null
  @Before
  def storeXsource(): Unit = {
    storedXsource = settings.source.value
  }
  @After
  def restoreXsource(): Unit = {
    settings.source.value = storedXsource
  }

  @Test
  def isAsSpecificScala2(): Unit = {
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
      assertTrue(!typer.infer.isAsSpecific(XA, XB))
      assertTrue(typer.infer.isAsSpecific(XB, XA))

      // Invariant
      assertTrue(!typer.infer.isAsSpecific(YA, YB))
      assertTrue(!typer.infer.isAsSpecific(YB, YA))

      // Contravariant: Scala 2 treats subtype/least derived as most specific
      assertTrue(typer.infer.isAsSpecific(ZA, ZB))
      assertTrue(!typer.infer.isAsSpecific(ZB, ZA))

      // Inner contravariant: Scala 2 treats subtype/least derived as most specific
      assertTrue(typer.infer.isAsSpecific(LZA, LZB))
      assertTrue(!typer.infer.isAsSpecific(LZB, LZA))

      // Subtypes of contravaraint effectively unrelated
      assertTrue(!typer.infer.isAsSpecific(ZA, SubZB))
      assertTrue(!typer.infer.isAsSpecific(SubZB, ZA))

      // Subtypes of contravaraint effectively unrelated
      assertTrue(!typer.infer.isAsSpecific(ZA, ZBwC))
      assertTrue(!typer.infer.isAsSpecific(ZBwC, ZA))

      // Contravariant outer subtypes
      assertTrue(!typer.infer.isAsSpecific(ZA, QuuxA))
      assertTrue(typer.infer.isAsSpecific(QuuxA, ZA))

      assertTrue(!typer.infer.isAsSpecific(ZA, QuuxB))
      assertTrue(!typer.infer.isAsSpecific(QuuxB, ZA))

      assertTrue(!typer.infer.isAsSpecific(ZB, QuuxA))
      assertTrue(typer.infer.isAsSpecific(QuuxA, ZB))

      assertTrue(!typer.infer.isAsSpecific(ZB, QuuxB))
      assertTrue(typer.infer.isAsSpecific(QuuxB, ZB))
    }
  }

  @Test
  def isAsSpecificScala3(): Unit = {
    settings.source.value = ScalaVersion("3.0")

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
      assertTrue(!typer.infer.isAsSpecific(XA, XB))
      assertTrue(typer.infer.isAsSpecific(XB, XA))

      // Invariant
      assertTrue(!typer.infer.isAsSpecific(YA, YB))
      assertTrue(!typer.infer.isAsSpecific(YB, YA))

      // Contravariant: treat top level as covariant
      assertTrue(!typer.infer.isAsSpecific(ZA, ZB))
      assertTrue(typer.infer.isAsSpecific(ZB, ZA))

      // Inner contravariant: also changed
      assertTrue(!typer.infer.isAsSpecific(LZA, LZB))
      assertTrue(typer.infer.isAsSpecific(LZB, LZA))

      // Subtypes of contravaraint effectively unrelated
      assertTrue(!typer.infer.isAsSpecific(ZA, SubZB))
      assertTrue(typer.infer.isAsSpecific(SubZB, ZA))

      // Subtypes of contravaraint effectively unrelated
      assertTrue(!typer.infer.isAsSpecific(ZA, ZBwC))
      assertTrue(typer.infer.isAsSpecific(ZBwC, ZA))

      // Contravariant outer subtypes
      assertTrue(!typer.infer.isAsSpecific(ZA, QuuxA))
      assertTrue(typer.infer.isAsSpecific(QuuxA, ZA))

      assertTrue(!typer.infer.isAsSpecific(ZA, QuuxB))
      assertTrue(typer.infer.isAsSpecific(QuuxB, ZA))

      assertTrue(!typer.infer.isAsSpecific(ZB, QuuxA))
      assertTrue(!typer.infer.isAsSpecific(QuuxA, ZB))

      assertTrue(!typer.infer.isAsSpecific(ZB, QuuxB))
      assertTrue(typer.infer.isAsSpecific(QuuxB, ZB))
    }
  }
}

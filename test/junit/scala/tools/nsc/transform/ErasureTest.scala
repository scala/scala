package scala.tools.nsc.transform

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting.{assertNoInvoke, getMethod}

class ErasureTest extends BytecodeTesting {
  object symbolTable extends SymbolTableForUnitTesting

  type Id[A] = A
  trait Tag[T] extends Any
  type @@[A, T] <: A with Tag[T]

  @Test def testGenericArray(): Unit = {
    import symbolTable._
    import definitions._
    val T = NoSymbol.newTypeParameter(TypeName("T")).setInfo(TypeBounds.empty)
    val U = NoSymbol.newTypeParameter(TypeName("U")).setInfo(TypeBounds.empty)
    val arrayTWithArrayString: Type = refinedType(appliedType(ArrayClass, T.tpeHK) :: appliedType(ArrayClass, StringTpe) :: Nil, NoSymbol)
    val arrayTWithArrayU: Type = refinedType(appliedType(ArrayClass, T.tpeHK) :: appliedType(ArrayClass, U.tpeHK) :: Nil, NoSymbol)

    assertEquals(1, erasure.unboundedGenericArrayLevel(appliedType(ArrayClass, T.tpeHK)))
    assertEquals(0, erasure.unboundedGenericArrayLevel(arrayTWithArrayString))
    assertEquals(1, erasure.unboundedGenericArrayLevel(arrayTWithArrayU))
  }

  import compiler._

  @Test
  def noScalaRuntimeForRefinedTypeWithGenericAndConcreteArray(): Unit = {
    val code =
      """class C {
        |  def t[T](xs: Array[T]): Unit = xs match {
        |    case x: Array[AnyRef] => x(0)
        |    case _ =>
        |  }
        |}
      """.stripMargin
    val c = compileClass(code)
    assertNoInvoke(getMethod(c, "t"))
  }

  @Test
  def testHigherKindedTypeAlias(): Unit = {
    import symbolTable._
    import definitions._

    assertEquals(StringTpe, erasure.scalaErasure(typeOf[Id[String]]))
  }

  @Test
  def testHigherKindedAbstractType(): Unit = {
    import symbolTable._
    import definitions._

    assertEquals(IntTpe, erasure.scalaErasure(weakTypeOf[Int @@ String]))
  }
}

package scala.tools.nsc.transform

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting.{assertNoInvoke, getMethod}

class ErasureTest extends BytecodeTesting {
  object symbolTable extends SymbolTableForUnitTesting

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
}

package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith

import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SymbolTableTest {
  object symbolTable extends SymbolTableForUnitTesting

  @Test
  def initDefinitions(): Unit = {
    symbolTable.definitions.init()
  }

  @Test
  def basicSubTypeCheck(): Unit = {
    symbolTable.definitions.init()
    val listClassTpe = symbolTable.definitions.ListClass.tpe
    val seqClassTpe = symbolTable.definitions.SeqClass.tpe
    assertTrue("List should be subclass of Seq", listClassTpe <:< seqClassTpe)
  }

  /**
   * Demonstrates how one can create symbols and type completely
   * from scratch and perform sub type check.
   */
  @Test
  def customClassesSubTypeCheck(): Unit = {
    import symbolTable._
    symbolTable.definitions.init()
    val rootClass = symbolTable.rootMirror.RootClass
    val fooSymbol = rootClass.newClassSymbol(TypeName("Foo"), NoPosition, 0)
    val fooType = new ClassInfoType(Nil, EmptyScope, fooSymbol)
    fooSymbol.info = fooType
    val barSymbol = rootClass.newClassSymbol(TypeName("Bar"), NoPosition, 0)
    val fooTypeRef = TypeRef(fooSymbol.owner.tpe, fooSymbol, Nil)
    val barType = new ClassInfoType(List(fooTypeRef), EmptyScope, barSymbol)
    barSymbol.info = barType
    assertTrue("Bar should be subclass of Foo", barSymbol.tpe <:< fooSymbol.tpe)
    assertFalse("Foo should be a superclass of Foo", fooSymbol.tpe <:< barSymbol.tpe)
  }

  @Test
  def noSymbolOuterClass_t9133(): Unit = {
    import symbolTable._
    assertEquals(NoSymbol, NoSymbol.outerClass)
  }

  @Test def t12702_glb(): Unit = {
    import symbolTable._
    import SymbolTableTest.t12702._
    val t1 = typeOf[IOS.type]
    val t2 = {
      val sd = typeOf[SD]
      sd.memberType(sd.member(TermName("mFI"))).finalResultType
    }
    // t1: Test.IOS.type
    // t2: Test.MFI{type +ST = S}

    // Ends up in `throw GlbFailure` in glb => Null
    assertTrue(definitions.NullTpe =:= glb(t1 :: t2 :: Nil))
  }

  @Test def invariantLub(): Unit = {
    import SymbolTableTest.invariantLub._
    import symbolTable._
    val l = lub(typeOf[C[String]] :: typeOf[C[Nothing]] :: Nil)
    assertTrue(s"$l", l =:= typeOf[C[String]])
  }
}

object SymbolTableTest {
  object invariantLub {
    class C[T]
  }

  object t12702 {
    import scala.language.existentials
    trait MFSS[X <: MFSS[_]]
    trait CS extends MFSS[CS]
    trait MFI { type ST }
    case class MFSD[S](mFI: MFI {type ST = S})
    case object IOS extends MFI { type ST = CS }
    type SD = MFSD[S] forSome {
      type S <: MFSS[S]
    }
  }
}

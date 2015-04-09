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
  def initDefinitions = {
    symbolTable.definitions.init()
  }

  @Test
  def basicSubTypeCheck = {
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
  def customClassesSubTypeCheck: Unit = {
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
  def noSymbolOuterClass_t9133: Unit = {
    import symbolTable._
    assert(NoSymbol.outerClass == NoSymbol)
  }
}

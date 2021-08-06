package scala.tools.nsc
package symtab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

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
    assertTrue(listClassTpe <:< seqClassTpe, "List should be subclass of Seq")
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
    assertTrue(barSymbol.tpe <:< fooSymbol.tpe, "Bar should be subclass of Foo")
    assertFalse(fooSymbol.tpe <:< barSymbol.tpe, "Foo should be a superclass of Foo")
  }

  @Test
  def noSymbolOuterClass_t9133(): Unit = {
    import symbolTable._
    assertEquals(NoSymbol, NoSymbol.outerClass)
  }
}

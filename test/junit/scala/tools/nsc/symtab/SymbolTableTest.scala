package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith

import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SymbolTableTest {
  private def createSymbolTable: SymbolTable = new SymbolTableForUnitTesting

  @Test
  def initDefinitions = {
    val symbolTable = createSymbolTable
    symbolTable.definitions.init()
  }

  @Test
  def basicSubTypeCheck = {
    val symbolTable = createSymbolTable
    symbolTable.definitions.init()
    val listClassTpe = symbolTable.definitions.ListClass.tpe
    val seqClassTpe = symbolTable.definitions.SeqClass.tpe
    assertTrue("List should be subclass of Seq", listClassTpe <:< seqClassTpe)
  }

}

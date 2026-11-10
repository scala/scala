package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class BaseTypeSeqsTest {
  object symbolTable extends SymbolTableForUnitTesting {
    /** As if the `Int` counter of symbol ids had wrapped around and come back to `id`. */
    def nextIdWillBe(id: Int): Unit = ids = id - 1
  }
  import symbolTable._, definitions._

  /** In a long-lived `Global`, symbol ids repeat after about four billion symbols, including the ids of symbols that
   *  live as long as the `Global`, such as `AnyClass`, which every base type sequence contains.
   */
  @Test def baseTypeIndexComparesSymbolsNotIds(): Unit = {
    def cls(name: String): ClassSymbol = {
      val c = rootMirror.RootClass.newClassSymbol(TypeName(name), NoPosition, 0L)
      c.setInfo(ClassInfoType(List(AnyRefTpe), newScope, c))
      c
    }
    val c = cls("C")
    symbolTable.nextIdWillBe(AnyClass.id)
    val d = cls("D")
    assertEquals(AnyClass.id, d.id)

    assertTrue(c.isSubClass(AnyClass))
    assertEquals(-1, c.info.baseTypeIndex(d))
    assertFalse(c.isSubClass(d))
    assertFalse(c.tpe <:< d.tpe)
  }
}

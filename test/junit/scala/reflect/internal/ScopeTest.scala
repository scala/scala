package scala.reflect.internal

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class ScopeTest {
  object symbolTable extends SymbolTableForUnitTesting

  import symbolTable._

  @Test
  def testNestedScopeSmall(): Unit = testNestedScope(0)
  @Test
  def testNestedScopeLarge(): Unit = testNestedScope(64) // exceeding MIN_HASH

  private def testNestedScope(initSize: Int): Unit = {
    def sym(termName: String): Symbol = NoSymbol.newValue(TermName(termName))
    val foo = sym("foo")
    val bar = sym("bar")

    val outerElems = List.tabulate(initSize)(i => sym(i.toString))
    val outer = newScopeWith(outerElems ++ List(foo, bar) : _*)
    assertTrue(outer.containsName(foo.name))
    assertTrue(outer.containsName(bar.name))

    val baz = sym("baz")
    val nested = newNestedScope(outer)

    // Entries from the outer scope are entered in the nested.
    assertTrue(outer.containsName(foo.name))
    assertTrue(outer.containsName(bar.name))

    // Nested scopes structurally share ScopeEntry-s with the outer.
    assertSame(outer.lookupEntry(foo.name), nested.lookupEntry(foo.name))
    nested.enter(baz)

    // Symbols entered in the nested scope aren't visible in the outer.
    assertTrue(nested.containsName(baz.name))
    assertTrue(!outer.containsName(baz.name))

    // Unlinking a symbol in the inner scope doesn't modify the outer
    nested.unlink(bar)
    assertFalse(nested.containsName(bar.name))
    assertTrue(outer.containsName(bar.name))
  }
}

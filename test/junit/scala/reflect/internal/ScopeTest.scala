package scala.reflect.internal

import scala.tools.nsc.symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil.assertThrows
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class ScopeTest {
  object symbolTable extends SymbolTableForUnitTesting

  import symbolTable._

  @Test
  def testNestedScopeSmall(): Unit = testNestedScope(0)
  @Test
  def testNestedScopeLarge(): Unit = testNestedScope(64) // exceeding MIN_HASH

  private def testNestedScope(initSize: Int) {
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
    assert(!nested.containsName(bar.name))
    assert(outer.containsName(bar.name))
  }
}

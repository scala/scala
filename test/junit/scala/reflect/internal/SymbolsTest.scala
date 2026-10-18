package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class SymbolsTest {

  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  private def overloaded(n: Int): (Symbol, List[Symbol]) = {
    val owner = rootMirror.EmptyPackageClass.newClassSymbol(TypeName("C"))
    val alts = List.tabulate(n) { i =>
      val m = owner.newMethod(TermName("m"))
      m.setInfo(MethodType(m.newSyntheticValueParams(List.fill(i)(IntTpe)), IntTpe))
    }
    (owner.newOverloaded(owner.thisType, alts), alts)
  }

  @Test
  def filterOverloadedAllPass(): Unit = {
    val (sym, alts) = overloaded(4)
    assertTrue(sym.isOverloaded)
    // No new symbol (or list) when nothing is filtered out. This relies on List.filter returning
    // the original list when every element satisfies the predicate.
    assertSame(sym, sym.filter(_ => true))
    assertSame(sym.alternatives, sym.filter(alts.contains).alternatives)
  }

  @Test
  def filterOverloadedPartial(): Unit = {
    val (sym, alts) = overloaded(5)
    val evaluated = List.newBuilder[Symbol]
    val result = sym.filter { alt => evaluated += alt; alt != alts(1) && alt != alts(3) }
    assertEquals(alts, evaluated.result())
    assertTrue(result.isOverloaded)
    assertEquals(List(alts(0), alts(2), alts(4)), result.alternatives)

    assertSame(alts(2), sym.filter(_ == alts(2)))
    assertSame(NoSymbol, sym.filter(_ => false))
  }

  @Test
  def filterNonOverloaded(): Unit = {
    val (_, alts) = overloaded(1)
    val m = alts.head
    assertSame(m, m.filter(_ => true))
    assertSame(NoSymbol, m.filter(_ => false))
  }
}

package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.runtime.ScalaRunTime
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class TreeGenTest {
  object symbolTable extends SymbolTableForUnitTesting

  import symbolTable._

  @Test
  def attributedRefToTopLevelMemberNotPrefixedByThis_t9473_a(): Unit = {
    val SomeClass = symbolOf[Some[_]]
    val ref = gen.mkAttributedRef(SomeClass)
    assertEquals("scala.Some", ref.toString) // was scala.this.Some
    ref match {
      case sel @ Select(pre @ Ident(preName), name) =>
      assertEquals(TermName("scala"), preName)
        assertEquals(TypeName("Some"), name)
        assertEquals(SomeClass, sel.symbol)
      case _ => fail(showRaw(ref))
    }
  }

  @Test
  def attributedRefToTopLevelMemberNotPrefixedByThis_t9473_b(): Unit = {
    val ScalaRuntimeModule = symbolOf[ScalaRunTime.type].sourceModule
    val ref = gen.mkAttributedRef(ScalaRuntimeModule)
    assertEquals("scala.runtime.ScalaRunTime", ref.toString)
    ref match {
      case sel @ Select(Select(Ident(TermName("scala")), TermName("runtime")), TermName("ScalaRunTime")) =>
      case _ => fail(showRaw(ref))
    }
  }
  @Test
  def attributedRefToTopLevelMemberNotPrefixedByThis_t9473_c(): Unit = {
    val DummyImplicitClass = symbolOf[Predef.DummyImplicit]
    val ref = gen.mkAttributedRef(DummyImplicitClass)
    assertEquals("scala.Predef.DummyImplicit", ref.toString)
//    ref match {
//      case sel @ Select(Select(Ident(TermName("scala")), TermName("runtime")), TermName("ScalaRunTime")) =>
//      case _ => fail(showRaw(ref))
//    }
  }
}

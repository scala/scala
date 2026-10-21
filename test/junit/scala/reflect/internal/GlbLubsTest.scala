package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class GlbLubsTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  /** Classes `B` and `C` extending `P`, all with a member `f`. Lubbing `B` and `C` forces the
   *  members' infos and checks they conform to `P.f`.
   */
  private def classes(pf: Type, bf: Type => Type, cf: Type => Type): List[Type] = {
    val owner = rootMirror.RootClass
    def cls(name: String, parent: Type, fInfo: Type): Symbol = {
      val c = owner.newClassSymbol(TypeName(name), NoPosition, 0L)
      val f = c.newMethod(TermName("f"), NoPosition, 0L)
      c.setInfo(ClassInfoType(List(parent), newScopeWith(f), c))
      f.setInfo(fInfo)
      c
    }
    val p = cls("P", AnyRefTpe, NullaryMethodType(pf))
    List(cls("B", p.tpe, bf(p.tpe)), cls("C", p.tpe, cf(p.tpe))).map(_.tpe)
  }

  /** A top-level lub nested in another lub, here run by a completer, must keep the outer lub's
   *  recursion guard. The outer lub can be top-level or have an explicit depth, as when called
   *  from `solve` or `mergePrefixAndArgs`.
   */
  @Test def nestedLubKeepsOuterGuard(): Unit = {
    def check(outer: List[Type] => Type, outerDepth: List[Type] => Depth): Unit = {
      var ts: List[Type] = Nil
      var guardKept: Option[Boolean] = None
      val completer = new LazyType {
        override def complete(sym: Symbol): Unit = {
          lub(List(IntTpe, StringTpe))
          guardKept = Some(lubResults.get((outerDepth(ts), ts)).contains(AnyTpe))
          sym.setInfo(NullaryMethodType(AnyTpe))
        }
      }
      ts = classes(AnyTpe, _ => completer, NullaryMethodType(_))
      outer(ts)
      assertEquals(Some(true), guardKept)
      assertTrue(lubResults.isEmpty)
    }
    check(lub(_), lubDepth)
    check(lub(_, Depth(3)), _ => Depth(3))
  }

  /** A top-level lub that re-enters itself through something other than a completer, which
   *  would detect the cycle, must still terminate.
   */
  @Test def selfReentrantLubTerminates(): Unit = {
    val annotated = AnnotatedType(List(AnnotationInfo(TypeConstraintClass.tpe, Nil, Nil)), IntTpe)
    val ts = classes(annotated, _ => NullaryMethodType(IntTpe), _ => NullaryMethodType(IntTpe))
    val limit = 20
    var nesting, maxNesting = 0
    addAnnotationChecker(new AnnotationChecker {
      def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
        if (nesting < limit) {
          nesting += 1
          maxNesting = maxNesting max nesting
          try lub(ts) finally nesting -= 1
        }
        true
      }
    })
    try lub(ts)
    finally removeAllAnnotationCheckers()
    assertTrue(s"lub re-entered itself $maxNesting times", maxNesting < limit)
  }
}

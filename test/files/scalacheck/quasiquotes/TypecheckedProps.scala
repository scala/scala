import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._, build.{Ident => _, _}

object TypecheckedProps extends QuasiquoteProperties("typechecked") {
  def original(tree: Tree) = tree match {
    case tt: TypeTree => Some(tt.original)
    case _            => None
  }
  def originals(trees: List[Tree]) = trees.flatMap(original)
  val int = ScalaDot(TypeName("Int"))
  val intint = List(int, int)

  property("tuple term") = test {
    val q"(..$elements)" = typecheck(q"(1, 2)")
    assert(elements ≈ List(q"1", q"2"))
  }

  property("tuple type") = test {
    val tq"(..$els0)" = typecheckTyp(tq"Unit")
    assert(els0.isEmpty)
    val tq"(..$els1)" = typecheckTyp(tq"(Int, Int)")
    assert(originals(els1) ≈ intint)
  }

  property("function type") = test {
    val tq"(..$argtpes) => $restpe" = typecheckTyp(tq"(Int, Int) => Int")
    assert(originals(argtpes) ≈ intint)
    assert(original(restpe).get ≈ int)
  }
}
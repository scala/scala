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

  property("for/for-yield") = test {
    val enums = fq"x <- xs" :: fq"x1 = x + 1" :: fq"if x1 % 2 == 0" :: Nil
    val body = q"x1"
    val xs = q"val xs = List(1, 2, 3)"
    val q"$_; for(..$enums0) yield $body0" = typecheck(q"$xs; for(..$enums) yield $body")
    assert(enums0 ≈ enums)
    assert(body0 ≈ body)
    val q"$_; for(..$enums1) $body1" = typecheck(q"$xs; for(..$enums) $body")
    assert(enums1 ≈ enums)
    assert(body1 ≈ body)
  }

  property("for .filter instead of .withFilter") = test {
    val enums = fq"foo <- new Foo" :: fq"if foo != null" :: Nil
    val body = q"foo"
    val q"$_; for(..$enums1) yield $body1" = typecheck(q"""
      class Foo { def map(f: Any => Any) = this; def filter(cond: Any => Boolean) = this }
      for(..$enums) yield $body
    """)
    assert(enums1 ≈ enums)
    assert(body1 ≈ body)
  }

  property("extract UnApply (1)") = test {
    val q"object $_ { $_; $_; $m }" = typecheck(q"""
      object Test {
        class Cell(val x: Int)
        object Cell { def unapply(c: Cell) = Some(c.x) }
        new Cell(0) match { case Cell(v) => v }
      }
    """)
    val q"$_ match { case $f(..$args) => $_ }" = m
    assert(f ≈ pq"Test.this.Cell")
    assert(args ≈ List(pq"v"))
  }

  property("extract UnApply (2)") = test {
    val q"object $_ { $_; $m }" = typecheck(q"""
      object Test {
        case class Cell(val x: Int)
        new Cell(0) match { case Cell(v) => v }
      }
    """)
    val q"$_ match { case ${f: TypeTree}(..$args) => $_ }" = m
    assert(f.original ≈ pq"Test.this.Cell")
    assert(args ≈ List(pq"v"))
  }

  property("extract inferred val type") = test {
    val typechecked = typecheck(q"val x = 42")
    val q"val x = 42" = typechecked
    val q"val x: ${tq""} = 42" = typechecked
    val q"val x: ${t: Type} = 42" = typechecked
  }

  property("class with param (1)") = test {
    val paramName = TermName("x")
    val q"class $_($param)" = typecheck(q"class Test(val $paramName: Int)")

    assert(param.name == paramName)
  }

  property("class with param (2)") = test {
    val paramName = TermName("y")
    val q"{ class $_($param); $_}" = typecheck(q"{ class Test(val $paramName: Int = 3) }")

    assert(param.name == paramName)
    assert(param.rhs ≈ q"3")
  }

  property("class with params") = test {
    val pName1 = TermName("x1")
    val pName2 = TermName("x2")
    val q"{ class $_($param1)(..$params2); $_}" = typecheck(q"{ class Test(val x0: Float)(val $pName1: Int = 3, $pName2: String) }")

    val List(p1, p2, _*) = params2

    assert(p1.name == pName1)
    assert(p2.name == pName2)
    assert(params2.size == 2)
  }
}

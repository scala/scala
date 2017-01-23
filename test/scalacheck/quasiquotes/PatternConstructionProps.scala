import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object PatternConstructionProps extends QuasiquoteProperties("pattern construction") {
  property("unquote bind") = forAll { (bind: Bind) =>
    pq"$bind" ≈ bind
  }

  property("unquote name into bind") = forAll { (name: TermName) =>
    pq"$name" ≈ Bind(name, Ident(termNames.WILDCARD))
  }

  property("unquote name and tree into bind") = forAll { (name: TermName, tree: Tree) =>
    pq"$name @ $tree" ≈ Bind(name, tree)
  }

  property("unquote type name into typed") = forAll { (name: TypeName) =>
    pq"_ : $name" ≈ Typed(Ident(termNames.WILDCARD), Ident(name))
  }

  property("unquote tree into typed") = forAll { (typ: Tree) =>
    pq"_ : $typ" ≈ Typed(Ident(termNames.WILDCARD), typ)
  }

  property("unquote into apply") = forAll { (pat: Tree, subpat: Tree) =>
    pq"$pat($subpat)" ≈ Apply(pat, List(subpat))
  }

  property("unquote into casedef") = forAll { (pat: Tree, cond: Tree, body: Tree) =>
    cq"$pat if $cond => $body" ≈ CaseDef(pat, cond, body)
  }

  property("unquote into alternative") = forAll { (first: Tree, rest: List[Tree]) =>
    pq"$first | ..$rest" ≈ Alternative(first :: rest)
  }
}

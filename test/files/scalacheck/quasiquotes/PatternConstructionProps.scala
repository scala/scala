import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object PatternConstructionProps extends QuasiquoteProperties("pattern construction") {
  property("splice bind") = forAll { (bind: Bind) =>
    pq"$bind" ≈ bind
  }

  property("splice name into bind") = forAll { (name: TermName) =>
    pq"$name" ≈ Bind(name, Ident(nme.WILDCARD))
  }

  property("splice name and tree into bind") = forAll { (name: TermName, tree: Tree) =>
    pq"$name @ $tree" ≈ Bind(name, tree)
  }

  property("splice type name into typed") = forAll { (name: TypeName) =>
    pq"_ : $name" ≈ Typed(Ident(nme.WILDCARD), Ident(name))
  }

  property("splice tree into typed") = forAll { (typ: Tree) =>
    pq"_ : $typ" ≈ Typed(Ident(nme.WILDCARD), typ)
  }

  property("splice into apply") = forAll { (pat: Tree, subpat: Tree) =>
    pq"$pat($subpat)" ≈ Apply(pat, List(subpat))
  }

  property("splice into casedef") = forAll { (pat: Tree, cond: Tree, body: Tree) =>
    cq"$pat if $cond => $body" ≈ CaseDef(pat, cond, body)
  }
}
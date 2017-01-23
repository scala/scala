import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object PatternDeconstructionProps extends QuasiquoteProperties("pattern deconstruction") {
  property("extract bind") = forAll { (bind: Bind) =>
    val pq"$bind0" = pq"$bind"
    bind0 ≈ bind
  }

  property("extract bind and subpattern") = forAll { (name: TermName, subp: Tree) =>
    val pq"$name0 @ $subp0" = pq"$name @ $subp"
    name0 ≈ name && subp0 ≈ subp
  }

  property("extract typed") = forAll { (typ: Tree) =>
    val pq"_ : $typ0" = pq"_ : $typ"
    typ0 ≈ typ
  }

  property("extract apply") = forAll { (pat: Tree, subpat: Tree) =>
    val pq"$pat0($subpat0)" = pq"$pat($subpat)"
    pat0 ≈ pat && subpat0 ≈ subpat
  }

  property("extract apply many") = forAll { (pat: Tree, subpats: List[Tree]) =>
    val pq"$pat0(..$subpats0)" = pq"$pat(..$subpats)"
    pat0 ≈ pat && subpats0 ≈ subpats
  }

  property("extract apply last") = forAll { (pat: Tree, subpats: List[Tree], subpatlast: Tree) =>
    val pq"$pat0(..$subpats0, $subpatlast0)" = pq"$pat(..$subpats, $subpatlast)"
    pat0 ≈ pat && subpats0 ≈ subpats && subpatlast0 ≈ subpatlast
  }

  property("extract casedef") = forAll { (pat: Tree, cond: Tree, body: Tree) =>
    val cq"$pat0 if $cond0 => $body0" = cq"$pat if $cond => $body"
    pat0 ≈ pat && cond0 ≈ cond && body0 ≈ body
  }

  property("extract alternative") = forAll { (first: Tree, rest: List[Tree]) =>
    val pq"$first1 | ..$rest1" = pq"$first | ..$rest"
    first1 ≈ first && rest1 ≈ rest
  }
}

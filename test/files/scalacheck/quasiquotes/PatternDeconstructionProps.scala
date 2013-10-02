import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._
import definitions._

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

  property("extract casedef") = forAll { (pat: Tree, cond: Tree, body: Tree) =>
    val cq"$pat0 if $cond0 => $body0" = cq"$pat if $cond => $body"
    pat0 ≈ pat && cond0 ≈ cond && body0 ≈ body
  }
}
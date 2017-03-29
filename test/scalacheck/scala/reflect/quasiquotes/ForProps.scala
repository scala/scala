package scala.reflect.quasiquotes

import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._, internal.reificationSupport._

object ForProps extends QuasiquoteProperties("for") {
  case class ForEnums(value: List[Tree])

  def genSimpleBind: Gen[Bind] =
    for(name <- genTermName)
      yield pq"$name @ _"

  def maybeWith(enumGen: Gen[Tree]): Gen[Tree] =
    for(enum <- enumGen; wrap <- Arbitrary.arbitrary[Boolean])
      yield if (wrap) fq"$enum with" else enum

  def genForFilter: Gen[Tree] =
    for(cond <- genIdent(genTermName))
      yield fq"if $cond"

  def genForFrom: Gen[Tree] =
    for(lhs <- genSimpleBind; rhs <- genIdent(genTermName))
      yield fq"$lhs <- $rhs"

  def genForEq: Gen[Tree] =
    for(lhs <- genSimpleBind; rhs <- genIdent(genTermName))
      yield fq"$lhs = $rhs"

  def genForEnums(size: Int): Gen[ForEnums] =
    for(first <- maybeWith(genForFrom); rest <- listOfN(size, maybeWith(oneOf(genForFrom, genForFilter, genForEq))))
      yield ForEnums(first :: rest)

  implicit val arbForEnums: Arbitrary[ForEnums] = arbitrarySized(genForEnums)

  object MaybeWith {
    def apply(tree: Tree, hasWith: Boolean): Tree =
      if (hasWith) fq"$tree with" else tree
    def unapply(tree: Tree): Option[(Tree, Boolean)] = tree match {
      case fq"$enum with" => Some((enum, true))
      case _ => Some((tree, false))
    }
  }

  // `with` keyword after Filter and ValEq is allowed, but all the Filters and ValEqs between two ValFroms must
  // have that keyword or otherwise it's ignored and lost during parsing
  private def stripRedundantWiths(enums: List[Tree]): List[Tree] = {
    def loop(enums: List[Tree], keepWithUp: Boolean): (List[Tree], Boolean) = enums match {
      case MaybeWith(t@fq"$pat <- $res", hasWith) :: rest =>
        val (tailRes, keepWithDown) = loop(rest, hasWith)
        (MaybeWith(t, hasWith && keepWithDown) :: tailRes, true)
      case MaybeWith(t, hasWith) :: rest =>
        val (tailRes, keepWithDown) = loop(rest, keepWithUp && hasWith)
        (MaybeWith(t, hasWith && keepWithUp && keepWithDown) :: tailRes, hasWith && keepWithDown)
      case Nil =>
        (Nil, false)
    }
    val (result, _) = loop(enums, keepWithUp = false)
    result
  }

  property("construct-reconstruct for") = forAll { (enums: ForEnums, body: Tree) =>
    try {
      val SyntacticFor(recoveredEnums, recoveredBody) = SyntacticFor(enums.value, body)
      recoveredEnums ≈ stripRedundantWiths(enums.value) && recoveredBody ≈ body
    } catch {
      case e => e.printStackTrace()
        throw e
    }
  }

  property("construct-reconstruct for-yield") = forAll { (enums: ForEnums, body: Tree) =>
    val SyntacticForYield(recoveredEnums, recoveredBody) = SyntacticForYield(enums.value, body)
    recoveredEnums ≈ stripRedundantWiths(enums.value) && recoveredBody ≈ body
  }

  val abcde = List(fq"a <-b", fq"if c", fq"d = e")
  val foobarbaz = pq"foo @ Bar(baz)"
  val fv = q"f(v)"

  property("construct/deconstruct for loop with fq") = test {
    val for0 = q"for(..$abcde) $fv"
    assertEqAst(for0, "for(a <- b; if c; d = e) f(v)")
    val q"for(..$enums) $body" = for0
    assert(enums ≈ abcde)
    assert(body ≈ fv)
  }

  property("construct/deconstruct valfrom with fq") = test {
    assert(fq"$foobarbaz <- $fv" ≈ fq"foo @ Bar(baz) <- f(v)")
    val fq"$lhs <- $rhs" = fq"$foobarbaz <- $fv"
    assert(lhs ≈ foobarbaz)
    assert(rhs ≈ fv)
  }

  property("construct/deconstruct valeq with fq") = test {
    assert(fq"$foobarbaz = $fv" ≈ fq"foo @ Bar(baz) = f(v)")
    val fq"$lhs = $rhs" = fq"$foobarbaz = $fv"
    assert(lhs ≈ foobarbaz)
    assert(rhs ≈ fv)
  }

  property("construct/deconstruct filter with fq") = test {
    assert(fq"if $fv" ≈ fq"if f(v)")
    val fq"if $cond" = fq"if $fv"
    assert(cond ≈ fv)
  }
}
import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._, internal.reificationSupport._

object ForProps extends QuasiquoteProperties("for") {
  case class ForEnums(val value: List[Tree])

  def genSimpleBind: Gen[Bind] =
    for(name <- genTermName)
      yield pq"$name @ _"

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
    for(first <- genForFrom; rest <- listOfN(size, oneOf(genForFrom, genForFilter, genForEq)))
      yield new ForEnums(first :: rest)

  implicit val arbForEnums: Arbitrary[ForEnums] = arbitrarySized(genForEnums)

  property("construct-reconstruct for") = forAll { (enums: ForEnums, body: Tree) =>
    val SyntacticFor(recoveredEnums, recoveredBody) = SyntacticFor(enums.value, body)
    recoveredEnums ≈ enums.value && recoveredBody ≈ body
  }

  property("construct-reconstruct for-yield") = forAll { (enums: ForEnums, body: Tree) =>
    val SyntacticForYield(recoveredEnums, recoveredBody) = SyntacticForYield(enums.value, body)
    recoveredEnums ≈ enums.value && recoveredBody ≈ body
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
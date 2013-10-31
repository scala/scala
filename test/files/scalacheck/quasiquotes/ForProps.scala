import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._, build.{Ident => _, _}

object ForProps extends QuasiquoteProperties("for") {
  case class ForEnums(val value: List[Tree])

  def genSimpleBind: Gen[Bind] =
    for(name <- genTermName)
      yield pq"$name @ _"

  def genForFilter: Gen[Tree] =
    for(cond <- genIdent(genTermName))
      yield SyntacticFilter(cond)

  def genForFrom: Gen[Tree] =
    for(lhs <- genSimpleBind; rhs <- genIdent(genTermName))
      yield SyntacticValFrom(lhs, rhs)

  def genForEq: Gen[Tree] =
    for(lhs <- genSimpleBind; rhs <- genIdent(genTermName))
      yield SyntacticValEq(lhs, rhs)

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
}
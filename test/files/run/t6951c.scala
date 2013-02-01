import scala.tools.partest._

object Test extends ReplTest {

  override def code = """
abstract class Tree {
  type FinalBranch <: Branch
  trait Branch {
    this: FinalBranch =>
    def myself: FinalBranch = this
  }
}
object Oak extends Tree {
  type FinalBranch = Branch
  class Branch extends super.Branch {
    this: FinalBranch =>
  }
}
object TypeConstructor {
  type ID[X] = X
  object Matcher {
    def unapply[M[X] <: ID[X], T <: Tree](branch: M[T#Branch]) = Some(branch.myself)
  }
  val oakBranch = new Oak.Branch
  val matched = oakBranch match {
    // Triggered Nil.head in `tp.baseType(tp.baseClasses.head)` in `widenToClass`
    // after correctly issueing an unchecked warning)
    case Matcher(branch) => branch
  }
}
"""
}




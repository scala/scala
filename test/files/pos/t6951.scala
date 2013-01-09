abstract class Tree {
  type FinalBranch <: Branch

  trait Branch {
    this: FinalBranch =>
    def myself: FinalBranch = this
  }
}

object Matcher {
  def unapply[T <: Tree](branch: T#Branch) = Some(branch.myself)
}

object Oak extends Tree {
  type FinalBranch = Branch
  class Branch extends super.Branch {
    this: FinalBranch =>
  }
}

object Caller extends App {
  val oakBranch = new Oak.Branch
  val matched = oakBranch match {
    case Matcher(branch) => branch // <-- type mismatch; found: T#Branch; required: Oak.Branch
  }
}

object CallerWithTag extends App {
  implicit def tag[T]: scala.reflect.ClassTag[T] = ???
  val oakBranch = new Oak.Branch
  val matched = oakBranch match {
    case Matcher(branch) => branch // <-- type mismatch; found: T#Branch; required: Oak.Branch
  }
}
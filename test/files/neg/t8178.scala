//> using options -Xfatal-warnings
sealed trait Fails
case class VarArgs1(a: String*) extends Fails
case class FailsChild2(a: Seq[String]) extends Fails
object FailsTest {
  def t1(f: Fails) = f match { // inexhaustive on both, was: no warning
    case VarArgs1(_)    => ???
  }

  def t2(f: Fails) = f match { // inexhaustive on VarArgs1
    case FailsChild2(_) => ???
  }

  def t12(f: Fails) = f match { // inexhaustive on VarArgs1, was: no warning
    case VarArgs1(_)    => ???
    case FailsChild2(_) => ???
  }

  def t21(f: Fails) = f match { // inexhaustive on VarArgs1, was: no warning
    case FailsChild2(_) => ???
    case VarArgs1(_)    => ???
  }

}

sealed trait Works
case class SeqArgs1(a: Seq[String]) extends Works
case class SeqArgs2(a: Seq[String]) extends Works
object WorksTest {
  def t12(f: Works) = f match {
    case SeqArgs1(_) => ???
    case SeqArgs2(_) => ???
  }

  def t1(f: Works) = f match { // inexhaustive on SeqArgs2
    case SeqArgs1(_) => ???
  }
}

//> using options -Werror

sealed case class Sub1(str: String)
final  case class Sup1(str: String) extends Sup0

final  class Sub2 extends Sub1("")
sealed trait Sup0 { def str: String }

// both of these unapplies are overloads of the synthetic unapply
// i.e. it isn't suppressed
object Sub1 { def unapply(x: Sub2): Some[String] = Some(x.str) }
object Sup1 { def unapply(x: Sup0): Some[String] = Some(x.str) }

object Test {
  // these seek the original unapplies and should be converted to use their constructors
  def testSub1(x: Sub1) = x match { case Sub1(str) => str }
  def testSup1(x: Sup1) = x match { case Sup1(str) => str }

  // these seek the user-defined alternative unapplies
  // thus they shouldn't accidentally be converted to use their constructors
  def testSub2(x: Sub2) = x match { case Sub1(str) => str }
  def testSup0(x: Sup0) = x match { case Sup1(str) => str }
}

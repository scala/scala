//> using options -deprecation -Xmigration -Werror

object UnicodeEscapes {
  def inTripleQuoted = """\u0041""" // deprecation
  def inRawInterpolation = raw"\u0041" // deprecation
  def inRawTripleQuoted = raw"""\u0041""" // deprecation
}

object InfixNewline extends App {
  class K { def x(y: Int) = 0 }

  def x(a: Int) = 1

  def ok = {
    (new K)
    `x` (42) // migration
  }
}

case class CaseCompanionMods private (x: Int) // nothing

trait InferredBase { def f: Object }
object InferredSub extends InferredBase { def f = "a" } // nothing

trait ExplicitImplicitsBase {
  implicit def b: String => Option[Int]
}
object ExplicitImplicits extends ExplicitImplicitsBase {
  implicit def b = _.toIntOption // error
  implicit val i = 0 // error
  implicit def s = "" // error
}

object AnyPlus { def f(xs: List[Int]) = xs + ";" }

object NameShadowing {
  class A { class X }
  class B extends A { class X; def f = new X }
}

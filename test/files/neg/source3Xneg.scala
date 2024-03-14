//> using options -deprecation -Xsource:3 -Xsource-features:_ -Wconf:cat=scala3-migration:w -Werror

// StringContext hygiene
class SC1 {
  class Impl(parts: Any*) {
    def s(args: Any*) = "hello, old world"
  }
  object StringContext {
    def apply(parts: Any*) = new Impl(parts: _*)
  }
  def name = "Scala3"
  def test = s"hello, $name" // ok
}

object UnicodeEscapes {
  def inTripleQuoted = """\u0041""" // ok
  def inRawInterpolation = raw"\u0041" // ok
  def inRawTripleQuoted = raw"""\u0041""" // ok
}

object InfixNewline extends App {
  class K { def x(y: Int) = 0 }

  def x(a: Int) = 1

  def ok = {
    (new K)
    `x` (42) // ok
  }
}

case class CaseCompanionMods private (x: Int) // ok
object CaseCompanionMods { def i = CaseCompanionMods(1) }

trait InferredBase { def f: Object }
object InferredSub extends InferredBase { def f = "a" } // nothing

trait ExplicitImplicitsBase {
  implicit def b: String => Option[Int]
}
object ExplicitImplicits extends ExplicitImplicitsBase {
  implicit def b = _.toIntOption // ok
  implicit val i = 0 // error
  implicit def s = "" // error
}

object AnyPlus { def f(xs: List[Int]) = xs + ";" }

object NameShadowing {
  class A { class X }
  class B extends A { class X; def f = new X }
}

object Test {
  locally {
    CaseCompanionMods.i.copy(CaseCompanionMods(2).x) // 2 errors
  }

  locally {
    InferredSub.f.toUpperCase // error
  }
}

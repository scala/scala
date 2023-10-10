//> using options -Xsource:3-cross

// StringContext hygiene
class SC1 {
  class Impl(parts: Any*) {
    def s(args: Any*) = "hello, old world"
  }
  object StringContext {
    def apply(parts: Any*) = new Impl(parts: _*)
  }
  def name = "Scala3"
  def test = s"hello, $name"
}

class SC2 {
  import SC2.*
  class Impl(parts: Any*) {
    def x(args: Any*) = "hello, old world" }
  object StringContext {
    def apply(parts: Any*) = new Impl(parts: _*)
  }
  def name = "Scala3"
  def test = x"hello, $name"
}
object SC2 {
  implicit class x(val sc: StringContext) extends AnyVal {
    def x(args: Any*) = "hello, world"
  }
}

object UnicodeEscapes {
  def inTripleQuoted = """\u0041"""
  def inRawInterpolation = raw"\u0041"
  def inRawTripleQuoted = raw"""\u0041"""
}

object InfixNewline extends App {
  class K { def x(y: Int) = 0 }

  def x(a: Int) = 1

  def ok = {
    (new K)
    `x` (42)
  }
}

case class CaseCompanionMods private (x: Int)
object CaseCompanionMods { def i = CaseCompanionMods(1) }

object Test extends App {
  locally {
    assert(new SC1().test == "hello, Scala3")
    assert(new SC2().test == "hello, world")
  }

  locally {
    val asList = List('\\', 'u', '0', '0', '4', '1')
    assert(asList == UnicodeEscapes.inTripleQuoted.toList)
    assert(asList == UnicodeEscapes.inRawInterpolation.toList)
    assert(asList == UnicodeEscapes.inRawTripleQuoted.toList)
  }

  locally {
    assert(InfixNewline.ok == 0)
  }

  locally {
    CaseCompanionMods.i
  }
}

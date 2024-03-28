// scalac: -Wunused:locals -Werror

class Outer {
  class Inner
}

trait Locals {
  def f0 = {
    var x = 1 // warn
    var y = 2 // no warn
    y = 3
    y + y
  }
  def f1 = {
    val a = new Outer // no warn
    val b = new Outer // warn
    new a.Inner
  }
  def f2 = {
    var x = 100 // warn about it being a var
    x
  }
}

object Types {
  def l1() = {
    object HiObject { def f = this } // warn
    class Hi { // warn
      def f1: Hi = new Hi
      def f2(x: Hi) = x
    }
    class DingDongDoobie // warn
    class Bippy // no warn
    type Something = Bippy // no warn
    type OtherThing = String // warn
    (new Bippy): Something
  }
}

// breakage: local val x$1 in method skolemize is never used
case class SymbolKind(accurate: String, sanitized: String, abbreviation: String) {
  def skolemize: SymbolKind = copy(accurate = s"$accurate skolem", abbreviation = s"$abbreviation#SKO")
}

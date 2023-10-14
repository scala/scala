
// scalac: -Xmigration -Xsource:3

trait T { def f: Object }
class K extends T { def f = "" }
object K {
  val k = new K
  val s: String = k.f
}

class F extends T { val f = "" }
object F {
  val f = new F
  val s: String = f.f
}

trait V extends T { var f = "" }
class W extends V
object W {
  val w = new W
  val s: String = w.f
}

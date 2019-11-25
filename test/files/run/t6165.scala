

class C(private[this] var c: String) {
  c = "changed"
  def f = c
}

object Test extends App {
  assert(new C("changeme").f == "changed")
}

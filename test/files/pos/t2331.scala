trait C {
  def m[T]: T
}

object Test {
  val o /*: C --> no crash*/ = new C {
    def m[T]: Nothing /*: T --> no crash*/ = error("omitted")
  }

  o.m[Nothing]
}
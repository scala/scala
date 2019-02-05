trait Main {
  trait C {
    def c(x: Any = 0)(bs: String*): Unit
  }
  def c: C
  c.c()(Seq[Any](): _*)
}

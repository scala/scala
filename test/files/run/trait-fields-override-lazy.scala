trait T {
  protected lazy val lv: Boolean = ???
}

object Test extends App {
  val overrideLazy = new T {
    override lazy val lv = true
    def foo = lv
  }

  assert(overrideLazy.foo)
  println("ok")
}

class Foo(val value: String) extends AnyVal {
  @annotation.tailrec final def loop(b: String): b.type = {
    loop(b)
  }

  def boppy() = {
    @annotation.tailrec def loop(x: value.type): Unit = loop(x)
  }
}

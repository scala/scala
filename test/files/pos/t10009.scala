class C {
  def c(a: Any, b: Any*) = a
}
object Test {
  new C().c(b = new { val x = 42 }, a = 0)
}

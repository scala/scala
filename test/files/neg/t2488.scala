class C {
  def f(a:Int, b:Int) = 1
  def f() = 2
}
object Test extends App {
  val c = new C()
  println(c.f(b = 2, 2))
  println(c.f(a = 2, c = 2))
  println(c.f(2, c = 2))
  println(c.f(c = 2, 2))
  println(c.f(2))
}

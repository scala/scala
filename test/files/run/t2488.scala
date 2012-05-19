class C {
  def f(a:Int, b:Int) = 1
  def f() = 2
}
object Test extends App {
  val c = new C()
  println(c.f(a = 1,2))
  println(c.f(a = 1, b = 2))
  println(c.f(b = 2, a = 1))
  println(c.f())
}

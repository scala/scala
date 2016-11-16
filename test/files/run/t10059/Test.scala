abstract class T {
  @annotation.varargs def m(l: Int*): Int
}
class C extends T {
  override def m(l: Int*): Int = 1
}
object Test extends App {
  assert(A.foo(new C) == 1)
}

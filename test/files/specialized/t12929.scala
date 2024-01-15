
class C[@specialized(Int) A](i: Int)

object Test extends App {
  val c = new C[Int](42)
  assert(c.getClass.getDeclaredFields.isEmpty)
}


object Test extends App {
  import J_1._
  import M._

  val res = f(classOf[C], FOO)
  assert(res == "class C, FOO")
}


object syntax {
  implicit class Ops1(x: String) { private[syntax] def bar: Int = 1 }
  implicit class Ops2(x: String) { def bar: Int = 2 }
}

object test {
  import syntax._
  val result = "foo".bar
}

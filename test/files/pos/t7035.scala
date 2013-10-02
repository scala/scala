case class Y(final var x: Int, final private var y: String, final val z1: Boolean, final private val z2: Any) {

  import Test.{y => someY}
  List(someY.x: Int, someY.y: String, someY.z1: Boolean, someY.z2: Any)
  someY.y = ""
}

object Test {
  val y = Y(0, "", true, new {})
  val unapp: Option[(Int, String, Boolean, Any)] = // was (Int, Boolean, String, Any) !!
    Y.unapply(y)

  val Y(a, b, c, d) = y
  List(a: Int, b: String, c: Boolean, d: Any)
}

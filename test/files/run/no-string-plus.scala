
object Test extends App {
  locally {
    import Stringly._
    val b = "false" + true
    Console println b
  }
  locally {
    import language.future.noStringPlus
    import Truly._

    val b = "false" + true
    Console println b
  }
}
object Truly {
  implicit class `sum of truths`(private val s: String) extends AnyVal {
    def +(b: Boolean) = s.toBoolean | b
  }
}
object Stringly {
  implicit class `sum of truths`(private val s: String) extends AnyVal {
    def +(other: Any) = s"$s$other"
  }
}

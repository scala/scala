
object Main extends App {

  def f(): Any = Macro {
    import Implicits._
    "world".greeting
  }

}

object Implicits {
  implicit class `strung out`(val s: String) {
    def greeting = s"hello, $s"
  }
}

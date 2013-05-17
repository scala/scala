
object Test extends App {
  val r = "(\\d+)".r
  List(1) collect { case r(i) => i }
}

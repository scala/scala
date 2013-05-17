object Test extends App {
  import Macros._
  def whatever() = null
  val q"$x1, $y1" = whatever()
  println(x1, y1)
  val q"$x2" = whatever()
  println(x2)
}

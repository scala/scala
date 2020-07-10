// scalac: -Yrangepos:false
object Test extends App {
  val num = 42
  println(Macros.pos(num + 17))
}

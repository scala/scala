object Test extends App {
  import Macros._
  val xs = cons(1, nil)
  println(xs)
  val ys = cons("abc", xs)
  println(ys)
}
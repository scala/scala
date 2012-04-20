object Test extends App {
  val x = Array[Unit]((), ())
  println(x.toString.substring(0, x.toString.indexOf(";")))
  println(x(0))
  x(1) = ()
  println(x(1))
}
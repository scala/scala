object Test extends App {
  println(identity('a').toInt)
  println('a'.toInt)
  // deprecated casts (char -> int)
  println(identity('a').asInstanceOf[Int])
  println('a'.asInstanceOf[Int])
  println(identity(1).asInstanceOf[Int])
  println(1.asInstanceOf[Int])
}

object Test extends Application {
  println(identity('a').toInt)
  println('a'.toInt)
  println(identity('a').asInstanceOf[int])
  println('a'.asInstanceOf[int])
  println(identity(1).asInstanceOf[int])
  println(1.asInstanceOf[int])
}

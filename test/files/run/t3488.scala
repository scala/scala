object Test extends App {
  def foo(p: => Unit)(x:Int = 0) = x

  println(foo { val List(_*)=List(0); 1 } ())
  println(foo { val List(_*)=List(0); 1 } (1))
}

package test

object A {
  println(("a" match {
    case "a" => 1
    case _ => "a"
  }).asInstanceOf[Object])
  def foo[T](x: T) = x
  var x: Int = 1
  var y: Long = 1L
  x = foo(x)
  y = foo(y)
}

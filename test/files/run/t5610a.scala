object Test extends App {
  class Result(_str: => String) {
    lazy val str = _str
  }

  def foo(str: => String)(i: Int) = new Result(str)

  def bar(f: Int => Result) = f(42)

  var test: String = null
  val result = bar(foo(test))
  test = "bar"

  if (result.str == null) {
    println("Destroy ALL THE THINGS!!!")
  } else {
    println("Stroke a kitten")
  }
}
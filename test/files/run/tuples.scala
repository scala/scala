import Function._

object Test extends App {
  var xyz: (Int, String, Boolean) = _
  xyz = (1, "abc", true)
  Console.println(xyz)
  xyz match {
    case (1, "abc", true) => Console.println("OK")
    case _ => ???
  }
  def func(x: Int, y: String, z: Double) {
    Console.println("x = " + x + "; y = " + y + "; z = " + z);
  }

  def params = (2, "xxx", 3.14159)  // (*****)

  tupled(func _)(params) // call the function with all the params at once
  func(2, "xxx", 3.14159) // the same call
  (func _).apply(2, "xxx", 3.14159) // the same call

  // Composing a tuple
  def t = (1, "Hello", false)

  // Decomposing a tuple
  val (i, s, b) = t

  // all the assertions are passed
  assert(i == 1)
  assert(s == "Hello")
  assert(b == false)
}

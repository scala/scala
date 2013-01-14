import Macros._

object Test extends App {
  println(unsound1(2, "42"))
  println(unsound2(2, "42"))
  println(unsound3(2, "42"))
  println(unsound4(2, "42", 42))
  println(unsound5(2, "42"))
}
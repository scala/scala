//> using options -Werror
class C {
  def m(x: true) = x match {
    case true => println("the one true path")
  }
}

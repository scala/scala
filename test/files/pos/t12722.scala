
// scalac: -Werror

class C {
  def f(n: Short) =
    n match {
      case -1 => true
      case _ => false
    }
}

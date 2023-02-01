
// scalac: -Werror -Xsource:3

class C {
  def f(n: Short) =
    n match {
      case 2147483647 => true
      case _ => false
    }
}

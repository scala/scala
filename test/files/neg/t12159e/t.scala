// javaVersion: 17+
// scalac: -Werror
package p

class C {
  def f(x: X) =
    x match {
      case y: Y => y.toString
      case z: Z => z.toString
    }
  def g(x: X) =
    x match {
      case w: W => w.toString
      case y: Y => y.toString
      case z: Z1 => z.toString
    }
}


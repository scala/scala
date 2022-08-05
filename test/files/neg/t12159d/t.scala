// javaVersion: 17+
// scalac: -Werror
package p

class C {
  def f(x: X) =
    x match {
      case y: Y => y.toString
      case z: Z => z.toString
    }
}


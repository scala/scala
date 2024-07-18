//> using jvm 17+
//> using options -Werror
package p

class C {
  def f(x: X) =
    x match {
      case y: Y => y.toString
      case z: Z => z.toString
    }
}


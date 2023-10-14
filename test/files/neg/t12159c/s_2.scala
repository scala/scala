// javaVersion: 17+
// scalac: -Werror
package p

class C {
  def f(h: H) =
    h match {
      case j: J => j.toString
      case l: L => l.toString
    }
}


// javaVersion: 17+
// scalac: -Werror
package p

class C {
  def f(h: H_1) =
    h match {
      case j: J => j.toString
      case k: K => k.toString
      case l: L => l.toString
    }
}


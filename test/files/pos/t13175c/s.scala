//> using jvm 17+
//> using options -Werror
package p

final class S extends C

class T {
  def f(c: C) = c match { case s: S => s; case j: J => j }
}

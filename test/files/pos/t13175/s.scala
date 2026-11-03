//> using jvm 17+
//> using options -Werror
package p

class S {
  def f(c: C) = c match { case e: B#E => e }
}

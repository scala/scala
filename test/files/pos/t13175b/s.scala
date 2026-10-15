//> using jvm 17+
//> using options -Werror
package p

class S {
  def f(c: C) = c match { case b: B => b; case e: B#E => e }
}

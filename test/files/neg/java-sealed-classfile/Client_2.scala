//> using jvm 17+
//> using options -Werror
package p

object Client {
  def f(s: S): Int = s match { case _: A => 0 }
}

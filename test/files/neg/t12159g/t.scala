//> using options -Werror -Xlint
package p
class T {
  def n(a: X) = a match { case _: Y => 42 }
}

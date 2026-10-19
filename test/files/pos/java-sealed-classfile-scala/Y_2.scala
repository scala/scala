//> using jvm 17+
//> using options -Werror
package p

// `X` is loaded from its classfile, whose `PermittedSubclasses` lists `Y`
final class Y extends X

object Client {
  def g(x: X): Int = x match { case _: Y => 0 }
}

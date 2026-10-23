//> using jvm 17+
//> using options -Werror
package p

// The children of `Poly`, read from a classfile, are known although nothing refers to them
object Test {
  def f(s: Shape) = s match { case _: Circle => 1; case _: Shape.Point => 2; case _: Holder.Inner => 3 }
}

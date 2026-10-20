//> using jvm 17+
//> using options -Werror
package p

// A.class still permits the removed `Holder.Inner`, `C` is the only child of `A`.
class T {
  def f(a: A) = a match { case c: C => c }
}

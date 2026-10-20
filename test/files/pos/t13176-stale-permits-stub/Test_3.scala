//> using jvm 17+
//> using options -Werror
package p

// Resolving `Holder.Inner` from A's InnerClasses table yields a stub symbol; it must not be an error
// to compute the children of `A`, and `C` is its only child.
class T {
  def f(a: A) = a match { case c: C => c }
}

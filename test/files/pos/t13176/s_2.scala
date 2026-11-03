//> using jvm 17+
//> using options -Werror
package p

class C {
  def f(i: Impl) = i
  def g(o: O) = o match { case i: Impl => i }
}

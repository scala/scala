trait FOO[B, m[A <: B]]
trait FOO2[A <: B, B]
trait FOO3[m[A <: B], B]

class Test {
  def foo[a[x]] = "a"
}
//trait Idiom[idi[x]] { def foo: idi[Int]}

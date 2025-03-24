//> using options -Ystop-after:refchecks
class C {
  class L[+A]
  def test = {
    val foo:
      L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_]]]]]]]]]]]]]]]]]]]]]]]]]
          = ??? } }


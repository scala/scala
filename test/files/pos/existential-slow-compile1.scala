class C {
  type L[+A] = scala.collection.immutable.List[A]
  def test = {
    val foo:
      L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: _ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: _ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_ <: L[_]]]]]]]]]]]]]]]]]]]]]]]]
          = ??? } }


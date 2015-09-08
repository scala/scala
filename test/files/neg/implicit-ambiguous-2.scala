object Test {
  trait =!=[C, D]

  implicit def neq[E, F] : E =!= F = null

  implicit def neqAmbig1[G, H, J] : J =!= J = null
  @annotation.implicitAmbiguous("Could not prove ${I} =!= ${I}")
  implicit def neqAmbig2[I] : I =!= I = null

  implicitly[Int =!= Int]
}

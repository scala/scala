trait Tc[F[_]]
object X {
  type Id[+A] = A
  val a: Tc[({type L[+X] = X})#L] = new Tc[({type L[+X] = X})#L] {} // ok, therefore the following should be too:
  val b: Tc[Id] = new Tc[Id] {} // ok
}

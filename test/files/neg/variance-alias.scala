trait Inv[A]

class X[+T] {
  type Id[+A] = A
  val a: Inv[({type L[+X] = X})#L[T]] = new Inv[({type L[+X] = X})#L[T]] {} // error
  val b: Inv[Id[T]] = new Inv[Id[T]] {} // error
}

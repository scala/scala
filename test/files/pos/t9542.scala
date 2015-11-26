object O {
  trait T

  class VC(val self: Any) extends AnyVal {
    def extMethod(f: F1[T, Any]) = ()
  }
}
trait F1[A, B]

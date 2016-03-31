trait F0[T] { def apply(): T }

object Test extends App {
  def delay[T](v: => T) = (v _): F0[T]

  // should not fail with ClassCastException: $$Lambda$6279/897871870 cannot be cast to F0
  // (also, should not say boe!)
  delay(println("boe!"))
}

class OC[T1: Ordering, T2: Ordering]() {
  val ordering = implicitly[Ordering[((Int, T1), T2)]]
}

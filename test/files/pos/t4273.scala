class A {
  class Bippy
  implicit val bippyOrdering: Ordering[Bippy] =
    new Ordering[Bippy] {
      def compare(x: Bippy, y: Bippy) = util.Random.nextInt
    }

  @deprecated("OrderingOps is deprecated", since="2.13.5")
  def test() = {
    // tests that (local) type inference of implicit type doesn't break it
    implicit def compareComparables[T](x: T)(implicit ord: Ordering[T]) = new ord.OrderingOps(x)

    new Bippy < new Bippy
  }
}

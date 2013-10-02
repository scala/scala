class A {
  implicit def compareComparables[T](x: T)(implicit ord: Ordering[T]) = new ord.Ops(x)

  class Bippy
  implicit val bippyOrdering = new Ordering[Bippy] { def compare(x: Bippy, y: Bippy) = util.Random.nextInt }

  (new Bippy) < (new Bippy)
}
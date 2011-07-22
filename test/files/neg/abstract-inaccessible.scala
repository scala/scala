package foo {
  private[foo] trait Bippy { }

  trait YourTrait {
    def implementMe(f: Int => (String, Bippy)): Unit
    def overrideMe[T <: Bippy](x: T): T = x
    def overrideMeAlso(x: Map[Int, Set[Bippy]]) = 5
  }
}

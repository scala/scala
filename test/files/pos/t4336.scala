object Main {
  class NonGeneric {}
  class Generic[T] {}

  class Composite {
    def contains(setup : Composite => Unit) : Composite = this
  }

  def generic[T](parent: Composite): Generic[T] = new Generic[T]
  def nonGeneric(parent: Composite): NonGeneric = new NonGeneric

  new Composite().contains(
    nonGeneric // should have type Composite => NonGeneric
  )

  new Composite().contains(
    generic[Int] // should have type Composite => Generic[Int]
  )
}

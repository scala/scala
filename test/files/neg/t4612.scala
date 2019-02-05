class t4612 {

  trait Ann[A] {
    def foo: A
  }

  class Bob extends Ann[Bob] {
    def foo = new Bob

    trait Cris extends Ann[Cris] {
      self: Bob =>

      def foo = new Bob
    }
  }
}

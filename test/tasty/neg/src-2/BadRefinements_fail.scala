package tastytest

object BadRefinements {

  class FooIntString extends Refinements.Foo {
    type T = String
    type U = Int
    def foo = ("I am foo", 23)
  }

  val b1 = new Refinements.Baz[Long, Boolean, FooIntString]

}

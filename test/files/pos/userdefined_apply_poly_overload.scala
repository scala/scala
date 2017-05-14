object Foo {
  // spurious error if:
  //   - this definition precedes that of apply (which is overloaded with the synthetic one derived from the case class)
  //   - AND `Foo.apply` is explicitly applied to `[A]` (no error if `[A]` is inferred)
  //
  def referToPolyOverloadedApply[A]: Foo[A] = Foo.apply[A]("bla")
  //                                                   ^
  //  found   : String("bla")
  //  required: Int

  def apply[A](x: Int): Foo[A] = ???
}
case class Foo[A](x: String) // must be polymorphic

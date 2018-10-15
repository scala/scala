object Test {
  trait Super[X]
  trait Template[T] {
    type Repr
    trait Sub extends Super[Repr]
  }

  // create a compound type that has a type variable in the decls of one of its parents
  implicit def reprTSub[T, Rpr[X]]: (Template[T]{type Repr = Rpr[T]})#Sub = ???
  implicitly[Super[Any]] // bug is not really related to implicit search, but is hard to trigger without
}
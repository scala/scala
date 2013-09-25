 trait T[T] {
    trait U {
      def x = ()
    }
    def u: U
  }

object Test {
  def foo = new T[Any] {
    def u: U = ???
  }

  // okay
  (foo: T[Any]).u.x

  // fails with if `TypeRef#coevolveSym` only looks in RefinedType's decls, rather than members.
  // This happens trying to existentially extrapolate the type `_2.U` (where `_2` is a refinement)
  // captured from `T.this.U`.
  //
  // This all comes about because `foo` returns a refinement type; if it were annotated with `T[Any]`
  // now existential extrapolation occurs.
  foo.u.x
}

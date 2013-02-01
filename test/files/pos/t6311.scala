class A {
  def fooMinimal[T, Coll <: Traversable[T]](msg: String)(param1: Traversable[T])(param2: Coll): Traversable[T] = throw new Exception()

  fooMinimal("")(List(1))(List(2))
}

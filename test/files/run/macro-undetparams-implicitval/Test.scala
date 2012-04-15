object Test extends App {
  def foo[T: TypeTag] = println(implicitly[TypeTag[T]])
  foo
}
object Test extends App {
  def foo[T: TypeTag] = Array[T]()
}
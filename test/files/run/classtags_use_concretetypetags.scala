object Test extends App {
  def foo[T: ConcreteTypeTag] = Array[T]()
}
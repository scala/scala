// test polymorphic nullary method calls
class A {
  // built-in
  synchronized {}

  val x: String = "a".asInstanceOf[String]

  // user-defined:
  def polyNullary[T]: List[T] = Nil
}

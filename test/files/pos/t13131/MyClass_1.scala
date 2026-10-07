package macros

final case class MyClass[T]() {
  def xyz = "xyz"
}

object MyClass extends MyClassMacros

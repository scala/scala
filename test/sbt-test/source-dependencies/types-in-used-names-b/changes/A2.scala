class A {
  type T <: S
  type S <: String
  def foo: T = null.asInstanceOf[T]
}

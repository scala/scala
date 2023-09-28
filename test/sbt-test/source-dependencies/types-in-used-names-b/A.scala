class A {
  type T <: S
  type S <: Int
  def foo: T = null.asInstanceOf[T]
}

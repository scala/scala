abstract class A {
  type T <: S
  type S
  object X {
    def foo: T = null.asInstanceOf[T]
  }
}

package issue14

trait P1[T] { def foo: T }
object test1 {
  class PWrapper[T] {
    @pkg.happytee val self: P1[T] = ???
  }
}
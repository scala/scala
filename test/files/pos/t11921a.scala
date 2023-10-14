
class C(x: Int) {
  class D extends C(42) {
    def f() = println(x)
  }
}

trait T {
  val t: Int
}
trait U extends T {
  val t: Int
  import t._
}
trait V { this: T =>
  val t: Int
  import t._
}

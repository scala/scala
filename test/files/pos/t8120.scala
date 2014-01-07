object A {
  class C {
    def m(a: Nothing): Int = 0
  }
  implicit class RichAny(a: Any) {
    def m(a: Any): Int = 0
  }
  (new C).m({ case (x, y) => x } : Any => Any)
}

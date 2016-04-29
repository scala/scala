class Test {
  implicit def s2i(s: String): Int = s.length
  trait Base { def foo: Int }
  trait Sub extends Base { val foo = "" }
}

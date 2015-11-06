class Test {
  trait Base { def foo: Int }
  trait Sub extends Base { val foo = "" }
}
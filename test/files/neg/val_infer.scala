class Test {
  trait Base { def foo: Int }
  trait Sub extends Base { def foo = "" }
}
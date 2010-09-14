object Test {
  class Foo
  class SomeOps(x : Foo) { def foo(x: String) = 1 }
  class OtherOps(x : Foo) { def foo(x: Int) = 1 }
  implicit def mkSomeOps(x: Foo) : SomeOps = new SomeOps(x)
  implicit def mkOtherOps(x: Foo) : OtherOps = new OtherOps(x)

  (new Foo).foo(1)
}
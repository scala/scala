// foo.scala
trait Foo {
    def foo(arg: List[_])
}
trait FooImpl extends Foo {
    def foo(arg: List[_]) {}
}
trait AbstractOverrideFoo extends Foo {
    abstract override def foo(arg: List[_]) {
        super.foo(arg)
    }
}

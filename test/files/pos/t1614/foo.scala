// foo.scala
trait Foo {
    def foo(arg: List[_]): Unit
}
trait FooImpl extends Foo {
    def foo(arg: List[_]): Unit = {}
}
trait AbstractOverrideFoo extends Foo {
    abstract override def foo(arg: List[_]): Unit = {
        super.foo(arg)
    }
}

object Test {
    def main(args: Array[String]): Unit = {
        val v: FooBarPlus[Int] = new FooBarPlusImpl()
        v.foo += 10
    }
}

trait Foo[P] {
    def foo: P
}

trait FooBar[P] extends Foo[P] {
    def bar: P
}

trait FooBarPlus[P] extends FooBar[P] {
    override def foo: P
    override def bar: P

    def foo_=(x: P): Unit
    def bar_=(x: P): Unit
}

class FooImpl extends Foo[Int] {
    def foo = 1
}

class FooBarImpl extends FooImpl with FooBar[Int] {
    protected var f = 0
    protected var b = 0

    override def foo = f
    def bar = b
}

class FooBarPlusImpl extends FooBarImpl with FooBarPlus[Int] {
    def foo_=(x: Int): Unit = { f = x }
    def bar_=(x: Int): Unit = { b = x }
}

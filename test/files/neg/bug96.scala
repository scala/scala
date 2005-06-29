class A {
    def foo(x: Any): Object = null;
}
class B extends A {
    override def foo(x: => Any): Object = null;
}

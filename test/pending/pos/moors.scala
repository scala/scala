object Test {

  implicit def foo2bar(foo :Foo) :Bar = foo.bar

  class Foo(val bar :Bar) {
    def testCoercion ={ val a: this.type = this; a.baz /* here, foo2bar is inferred  by the compiler, as expected */}
    def testCoercionThis = baz  // --> error: not found: value baz
    def testCoercionThis = (this: Foo).baz  // --> error: value baz is not a  member of Foo
  }
  
  class Bar { def baz = System.out.println("baz")} 
}

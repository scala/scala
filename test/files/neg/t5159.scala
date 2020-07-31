// scalac: -Yuntil:refchecks
//
abstract class A {
  type Foo
  def bar(foo: Foo): Unit
}

class BFoo

class CFoo extends BFoo

class B extends A {
  type Foo = BFoo
  def bar(foo: Foo): Unit = println("B called")
}

class C extends B {
  override type Foo = CFoo
  override def bar(foo: Foo): Unit = {
    super.bar(foo)
    println("C called")
  }
}

/*
bad-inherit.scala:18: error: ambiguous reference to overloaded definition,
both method bar in class B of type (foo: C.this.Foo): Unit
and  method bar in class A of type (foo: C.this.Foo): Unit
match argument types (C.this.Foo)
    super.bar(foo)
          ^
1 error
*/

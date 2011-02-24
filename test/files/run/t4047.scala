trait Foo[T] { val foo: T}

class A extends Foo[Unit]{
  lazy val foo = println("Unit: called A.foo")
}

class B extends Foo[Unit]{
  val foo = println("Unit: called B.foo")
}

trait Bar[T] { def foo: T}

class C extends Bar[Unit]{
  lazy val foo = println("Unit: called C.foo")
}

class D extends Bar[Unit]{
  def foo = println("Unit: called D.foo")
}

object Test extends Application {
  val a: Foo[Unit] = new A
  a.foo
  a.foo
  val b: Foo[Unit] = new B
  b.foo
  b.foo
  val c: Bar[Unit] = new C
  c.foo
  c.foo
  val d: Bar[Unit] = new D
  d.foo
  d.foo
}

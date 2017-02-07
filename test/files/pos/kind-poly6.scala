object Test {

  trait Foo
  class Foo1 extends Foo
  class Bar[T] extends Foo
  class Baz[T, U] extends Foo

  def fooFoo[T <: Foo with AnyKind]: Unit = ()
  fooFoo[Bar] // OK
  fooFoo[Baz] // OK
  // fooFoo[List] // Not OK
  // fooFoo[Int] // Not OK

  trait Toto1[A]
  class Toto11[A] extends Toto1[A]
  class Toto12[A, B] extends Toto1[A]

  def fooToto[T <: Toto1[_] with AnyKind]: Unit = ()
  fooToto[Toto11]
  fooToto[Toto12]
  // fooToto[List] // Not OK
  // fooToto[Int]   // Not OK

  def foo[T <: AnyKind]: Unit = ()
  foo[String]
  foo[Foo]
  foo[Bar]
  foo[Baz]
  foo[List]
  foo[Map]
  foo[List[Int]]
  foo[Map[Int, Long]]

}

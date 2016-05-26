package test

object Test {
  // A couple of type classes with type members ...
  trait Foo[T] {
    type A
  }

  object Foo {
    implicit val fooIS = new Foo[Int] { type A = String }
  }

  trait Bar[T] {
    type B
    val value: B
  }

  object Bar {
    implicit val barSB = new Bar[String] {
      type B = Boolean
      val value = true
    }
  }

  trait Baz[T]
  object Baz {
    implicit def baz[T]: Baz[T] = new Baz[T] {}
  }

  def run[T: Baz](t: T)(implicit foo: Foo[T])(implicit bar: Bar[foo.A]): bar.B = bar.value

  val value = run(23)
  assert(value: Boolean)

  val value2 = run(23)(Baz.baz, Foo.fooIS)(Bar.barSB)
  assert(value: Boolean)

  def boundNullary[T: Baz] = ()
  boundNullary[Int]
  boundNullary[Int](Baz.baz)

  def boundEmpty[T: Baz]() = ()
  boundEmpty[Int]()
  boundEmpty[Int]()(Baz.baz)

  def boundExplicit[T: Baz](i: T) = ()
  boundExplicit(23)
  boundExplicit[Int](23)(Baz.baz)

  def boundImplicit[T: Baz](implicit fooT: Foo[T]) = ()
  boundImplicit[Int]
  boundImplicit[Int](Baz.baz, Foo.fooIS)

  def boundExplicitImplicit[T: Baz](i: T)(implicit fooT: Foo[T]) = ()
  boundExplicitImplicit(23)
  boundExplicitImplicit[Int](23)(Baz.baz, Foo.fooIS)

  def boundEmptyImplicit[T: Baz]()(implicit fooT: Foo[T]) = ()
  boundEmptyImplicit[Int]()
  boundEmptyImplicit[Int]()(Baz.baz, Foo.fooIS)

  {
    def bar(a: Int)(implicit b: DummyImplicit)(implicit c: DummyImplicit) = 0
    val res = bar _
    (res: Int => Int)
  }

  {
    class A
    implicit def bar(implicit b: DummyImplicit)(implicit c: DummyImplicit): A = new A
    val res = implicitly[A]
    (res: A)
  }

  object Overload1 {
    def over(i: Int)(implicit d: DummyImplicit): Int = i
    def over(b: Boolean)(implicit d: DummyImplicit): Boolean = b

    (over(23): Int)
    (over(true): Boolean)
  }

  object Overload2 {
    def over(i: Int)(implicit d: DummyImplicit)(implicit e: DummyImplicit): Int = i
    def over(b: Boolean)(implicit d: DummyImplicit)(implicit e: DummyImplicit): Boolean = b

    (over(23): Int)
    (over(true): Boolean)
  }

  {
    object O {
      def a(implicit I: DummyImplicit) = 42
      def a_=(a: Any) = ()
    }
    O.a = 0
  }

  {
    object O {
      def a(implicit I: DummyImplicit)(implicit J: DummyImplicit) = 42
      def a_=(a: Any) = ()
    }
    O.a = 0
  }
}

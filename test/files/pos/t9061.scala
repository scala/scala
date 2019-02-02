object original {
  trait Bar[T] {
    val value: T
  }

  object Bar {
    def apply[T](t: => T): Bar[T] = new Bar[T] {
      val value = t
    }
  }

  trait Foo[A] { def foo(a: A): Unit }
  object Foo {
    implicit val intFoo = new Foo[Int] { def foo(x: Int) = () }
  }

  object Demo {
    Bar[Foo[Int]]({
      object Blah {
        lazy val blah: Foo[Int] = Foo.intFoo
      }
      Blah.blah
    }).value.foo _
  }
}

class Reduced {
  def byName(a: => Any) = ???
  byName({ object Blah; Blah }).toString _
}

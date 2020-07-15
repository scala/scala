package tastytest

object AnythingIsPossible {

  class Box[A](val a: A)

  class Class extends Box({ class X { final val x = Map(("", 3)) } ; val foo = new X(); foo.x: foo.x.type })

  class Lambda extends Box((x: Int) => (y: String) => y.length == x)

  object Zero {
    def unapply[A: Numeric](a: A): Boolean = implicitly[Numeric[A]].zero == a
  }

  type IntSpecial = Int @unchecked

  class Match extends Box((0: @unchecked) match {
    case n if n > 50    => "big"
    case 26 | 24        => "26 | 24"
    case a @ _ if a > 0 => "small"
    case Zero()         => "zero"
    case -1             => throw new IllegalArgumentException("-1")
    case _: IntSpecial  => "negative"
  })

  class While extends Box(while (false) {})

  class Try extends Box(try throw new IllegalArgumentException("nothing") catch { case e: Exception => e.getMessage() })

  class Assign extends Box({ var t = 0; t = t + 1 })

  trait SomeSuper {
    def foo: Double = 23.451
  }

  object Within extends SomeSuper {
    class Super extends Box(super[SomeSuper].foo)
  }

}

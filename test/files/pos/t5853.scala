






final class C(val x: Int) extends AnyVal {
  def ppp[@specialized(Int) T](y: T) = ()
}


class Foo {
  def f = new C(1) ppp 2
}


/* Original SI-5853 test-case. */

object Bippy {
  implicit final class C(val x: Int) extends AnyVal {
    def +++[@specialized T](y: T) = ()
  }
  def f = 1 +++ 2
}


/* Few more examples. */

final class C2(val x: Int) extends AnyVal {
  def +++[@specialized(Int) T](y: T) = ()
}


class Foo2 {
  def f = new C2(1) +++ 2
}


object Arrow {
  implicit final class ArrowAssoc[A](val __leftOfArrow: A) extends AnyVal {
    @inline def ->>[B](y: B): Tuple2[A, B] = Tuple2(__leftOfArrow, y)
  }

  def foo = 1 ->> 2
}


object SpecArrow {
  implicit final class ArrowAssoc[A](val __leftOfArrow: A) extends AnyVal {
    @inline def ->> [@specialized(Int) B](y: B): Tuple2[A, B] = Tuple2(__leftOfArrow, y)
  }

  def foo = 1 ->> 2
}

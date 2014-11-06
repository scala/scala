package test.nestedcov

sealed abstract class Outer[+A]
case class Let[+A](expr: Outer[Inner[A]]) extends Outer[A]

sealed abstract class Inner[+A]

sealed abstract class Outer2[+A, +B]
case class Let2[+A](expr: Outer2[Inner2[A], A]) extends Outer2[A, A]

sealed abstract class Inner2[+A]

sealed abstract class Outer3[+A, +B]
case class Let3[+A](expr: Outer3[A, A]) extends Outer3[A, A]

object NestedCov {
  def run[A](nc: Outer[A]) = nc match {
    case Let(expr) =>
      expr : Outer[Inner[A]]
  }

  def run2[A](nc: Outer2[A, A]) = nc match {
    case Let2(expr) =>
      expr : Outer2[Inner2[A], A]
  }

  def run3[A](nc: Outer3[A, A]) = nc match {
    case Let3(expr) =>
      expr : Outer3[A, A]
  }
}

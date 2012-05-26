import language._

object Test {
  class Foo[T](val x: T) ; object Foo { def unapply[T](x: Foo[T]) = Some(x.x) }
  def f1[T](x: Foo[T]) = x match { case Foo(y) => y }
  def f2[M[_], T](x: M[T]) = x match { case Foo(y) => y }

  case class Bar[T](x: T)
  def f3[T](x: Bar[T]) = x match { case Bar(y) => y }
  def f4[M[_], T](x: M[T]) = x match { case Bar(y) => y }
}
//
// ./b.scala:4: warning: non variable type-argument T in type pattern Test.Foo[T] is unchecked since it is eliminated by erasure
//   def f2[M[_], T](x: M[T]) = x match { case Foo(y) => y }
//                                                ^
// one warning found
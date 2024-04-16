//> using options -Werror
//

import annotation._

@implicitNotFound("No C of ${ A }")
class C[A]

trait X$Y
/* using the $$ separator for expanded names is unwise
trait X$$Y
trait X$$$Y
trait X$$$$Y
 */

trait Foo[A]

trait U[X, Y[_], Z[_, ZZ]] {
  class I[R] {
    def m[S](implicit @implicitNotFound("${X} ${Y} ${ Z } ${R} ${S} -- ${XX}.") i: Int) = ???
  }
}

class Test {
  def f[A: C] = ???
  f[X$Y]
/* using the $$ separator for expanded names is unwise
  f[X$$Y]
  f[X$$$Y]
  f[X$$$$Y]
 */
  f[Foo[Int]]

  def g[Aaa](implicit theC: C[Aaa]) = ???
  g[Foo[Int]]

  def h[Aaa](implicit @implicitNotFound("I see no C[${Aaa}]") theC: C[Aaa]) = ???
  h[Foo[Int]]

  val u = new U[String, List, ({type T[A, _] = List[C[_]]})#T] { }
  val i = new u.I[Int]
  i.m[Option[Long]]
}

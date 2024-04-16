//> using options -Xlint:implicit-not-found -Xfatal-warnings
//
package test

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Cannot construct a collection of type ${Too} with elements of type ${Elem} based on a collection of type ${From}.")
trait Meh[-From, +To]

@implicitNotFound(msg = "Cannot construct a collection of type ${To} ${Elem}.")
trait Meh2[-From, +To]

class C[T]
trait T {
  def m[Aaa](implicit @implicitNotFound("I see no C[${Uuh}]") theC: C[Aaa]) = ???
  def n[Aaa](implicit @implicitNotFound("I see no C[${Aaa}]") theC: C[Aaa]) = ???
}

trait U[X, Y[_], Z[_, ZZ]] {
  class I[R] {
    def m[S](implicit @implicitNotFound("${X} ${Y} ${ Z } ${R} ${S} -- ${XX} ${ZZ} ${ Nix }") i: Int) = ???
  }
}

package tastytest

import scala.language.experimental.erasedDefinitions

import scala.annotation.experimental

@experimental
object SaferExceptions {

  class DivByZero extends Exception

  erased class CanThrowCapability[-E <: Exception]

  infix type mayThrow[+A, +E <: Exception] = (erased CanThrowCapability[E]) ?=> A

  def safeDiv(x: Int, y: Int): Int mayThrow DivByZero = {
    if (y == 0) throw new DivByZero()
    else x / y
  }

}

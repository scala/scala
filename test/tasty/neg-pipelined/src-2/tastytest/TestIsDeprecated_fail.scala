package tastytest

import lib.IsDeprecated

object TestIsDeprecated extends scala.App {

  val iD = new IsDeprecated

  assert(IsDeprecated.foo == 23) // error: deprecated method foo
  assert(iD.bar == 23) // error: deprecated method bar

  val iDI = new IsDeprecated.Inner // error: deprecated class Inner

  assert(IsDeprecated.Inner.baz == 23) // error: deprecated class Inner
}

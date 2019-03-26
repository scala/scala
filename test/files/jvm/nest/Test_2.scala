//############################################################################
// Test Scala interaction with Java nested classes and static members.
//############################################################################

import nestpkg._;

object Test extends App {
  val x = nest_1.best.rest.test
  Console.println(x.inc(1))

  val o = new nest_1.best;
  val r = new nest_1.best.rest;
  Console.println(nest_1.best.rest.test.inc(2))
  Console.println(nest_1.best.rest.x)

  print("Instantiating public inner class: ")
  val outer = new nest_1
  val inn   = new outer.Inn(42)
  inn.doSomething
}

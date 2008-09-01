//############################################################################
// Test Scala interaction with Java nested classes and static members.
//############################################################################
// $Id: inner.scala 8902 2006-10-10 10:58:10 +0200 (Tue, 10 Oct 2006) mihaylov $

/** found in nest.jar, compiled from nest.java */
import nestpkg._;

object Test extends Application {
  val x = nest.best.rest.test
  Console.println(x.inc(1))

  val o = new nest.best;
  val r = new nest.best.rest;
  Console.println(nest.best.rest.test.inc(2))
  Console.println(nest.best.rest.x)

  print("Instantiating public inner class: ")
  val outer = new nest
  val inn   = new outer.Inn(42)
  inn.doSomething
}

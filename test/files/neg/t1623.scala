package test

trait A
trait B

class BImpl extends B {
  this: A =>
}

object Test2 extends App {
  val b = new BImpl
}

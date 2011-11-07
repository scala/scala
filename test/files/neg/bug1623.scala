package test

trait A 
trait B 

class BImpl extends B {
  this: A =>
}

object Test2 extends Application {
  val b = new BImpl
}

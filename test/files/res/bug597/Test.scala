package test;

abstract class Base {
  type A <: Ax;
  
  abstract class Ax {
    def a = null;
    def string = "A";
  }
}
trait ExtB extends Base {
  type A <: Ax;
  trait Ax extends super.Ax {
    def c = null;
    override def string = super.string + "C";
  }
}

trait ExtC extends /*ExtA with*/ ExtB {
  type A <: Ax;
  trait Ax extends super.Ax {
    a
    c
    def d = null;
    override def string = super.string + "D";
  }
}

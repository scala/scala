trait Iface {
  def x: Int
}

object A {
  def x: Int
}

object B {
  val x: Int
}

object C {
  var x: Int
}

object D {
  type X
}

object E extends Iface

class AnonCls {

  val a = new { def x: Int }
  val b = new { val x: Int }
  val c = new { var x: Int }
  val d = new { type X }
  val e = new Iface { val one = 1 }

}

class ShouldBeAbstract {
  def x: Int
}

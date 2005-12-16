abstract class A() {

  var x: Int

}

abstract class B() extends A() {

  var xx: Int = 0;

  def x = xx;
  def x_=(y: Int) = xx = y;
}


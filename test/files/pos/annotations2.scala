
class B[T](x: (T, T)) {
  def this(xx: (T, Any, Any)) = this((xx._1, xx._1))
}
class BAnn[T](x: (T, T)) extends scala.annotation.StaticAnnotation {
  def this(xx: (T, Any, Any)) = this((xx._1, xx._1))
}
class CAnn[T](x: (T, T)) extends scala.annotation.StaticAnnotation {
  def this(xx: Class[T]) = this((xx.newInstance(), xx.newInstance()))
}

class A1 {
  val b1 = new B((1, 2, 3))
  val b2 = new B((1, 2))
  val b3 = new B[Int]((1, 2, 3))
  val b4 = new B[Int]((1, 2))
}

class A2 {
  @BAnn((1, 2, 3)) val b1 = null
  @BAnn((1, 2)) val b2 = null
  @BAnn[Int]((1, 2, 3)) val b3 = null
  @BAnn[Int]((1, 2)) val b4 = null
}

class A3 {
  @CAnn(classOf[Int]) val b1 = null
  @CAnn((1, 2)) val b2 = null
  @CAnn[Int](classOf[Int]) val b3 = null
  @CAnn[Int]((1, 2)) val b4 = null
}

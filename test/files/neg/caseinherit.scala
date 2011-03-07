case class A(x: Int)
case class B(y: Int) extends A(y)
case object Bippy extends A(55)

class Innocent extends A(5)
case class Dingus(y: Int) extends Innocent
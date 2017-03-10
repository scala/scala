
case class A(a: Int, index: Int) {
  def apply(i: Int)(implicit ev: Int): A = new A(ev, i)
  def update(i: Int, value: Int): A = if (i == index) A(i, value) else A(i, 0)
  def +(j: Int) = a + j
}

case class B(a: Int, index: Int) {
  def apply(i: Int)(j: Int)(implicit ev: Int): B = new B(j + ev, i)
  def update(i: Int, value: Int): B = if (i == index) B(i, value) else B(i, 0)
  def +(j: Int) = a + j
}

object Test extends App {
  def checkA(x: A, y: Int, z: Int) = assert(x == A(y, z))
  def checkB(x: B, y: Int, z: Int) = assert(x == B(y, z))
  implicit def ev: Int = 8000
  val a = A(5, 1)
  checkA(a(1) = 10, 1, 10)
  checkA(a(1) += 20, 1, 8020)
  val b = B(5, 1)
  checkB(b(1)(5) += 20, 1, 8025)
}

/*
 A(1,10)
 A(1,8020) // was A(8000,0)
 B(1,8025)
 */

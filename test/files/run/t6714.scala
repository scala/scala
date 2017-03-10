
case class A(a: Int, index: Int) {
  def apply(i: Int)(implicit ev: Int): A = new A(ev, i)
  def update(i: Int, value: Int): A = if (i == index) A(i, value) else A(i, 0)
  def +(j: Int) = a + j
}

object Test extends App {
  def checkA(x: A, y: Int, z: Int) = assert(x == A(y, z))
  implicit def ev: Int = 8000
  val a = A(5, 1)
  checkA(a(1) = 10, 1, 10)
  checkA(a(1) += 20, 1, 8020)
}

/*
 A(1,10)
 A(1,8020) // was A(8000,0)
 */


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
  implicit def ev: Int = 8000
  val a = A(5, 1)
  a(1) = 10         // OK
  a(1) += 20        // OK
  a(1)(9000) += 20  // Not OK
  val b = B(5, 1)
  b(1)(5) += 20     // Not OK
}

object t {
  def f = { object C; case class C(); 1 }
  // pending: def g = { case class D(x: Int); object D; 2 }
  def h = { case class E(y: Int = 10); 3 }
}

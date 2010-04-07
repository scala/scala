class A (val i: Int) {
  def copy (i: Int = this.i): A = new A(i)
}

class B (val j: Int) extends A(1) {
  override def copy (j: Int = this.j): B = new B(j)
}
